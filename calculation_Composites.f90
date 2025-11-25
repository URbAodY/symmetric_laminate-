!此方法为对称层合板的情况。例如，铺层设计中的数字4399表示[0_4/90_1/45_9/-45_9]s层合板
!program表示程序开始，calculation_Composites为该程序的名称，该名称可自定义
!_为驼峰命名法的格式，这个方法只是为了让别人和自己更容易知道这个程序的功能，不这样命名也行
!缩进和空行不影响程序的运行，空格在不破坏关键字等的情况下，不影响程序的运行，Fortran不分大小写

!用module就可以省去重复声明变量的麻烦。（如果有多个子程序需要用到相同的变量）
module global
    implicit none
    integer :: count1,count2,material_total,i,layer_total
    integer,allocatable :: liebiao (:)
    integer, allocatable :: layer_arrangements(:) !储存所有排列方式
    
    type box2
        integer :: layer
        real :: Ex_, Ey_, Gxy_,x_G, y_G,x_y,G_y
    end type box2 !表示该数据类型已经结束定义


    type(box2) ,allocatable :: result (:)
    type(box2)::count3 !排序时用

end module global
!全局变量


module material_parameters_save
    implicit none
    type box
        character :: name         
        real :: E1_, E2_, V1_, G12_
    end type box !表示该数据类型已经结束定义

    type(box) ,allocatable :: jisuan (:) !记录材料的4个工程弹性常数
    !type(box) ,allocatable用来声明一个可变大小的数组，且数组类型为自定义box类型
end module material_parameters_save




program calculation_Composites 

    use global
    use material_parameters_save

    implicit none
    !implicit是用来设置默认类型的。如果implicit后为none，则没有声明的变量会被编译器视为错误
    !若无此设置，未声明的变量会被编译器以其默认规则来自动确定该变量类型
    !很可能出现程序可以运行，但结果不合预期的情况
    

    !real的意思是声明一块保存浮点数的内存空间，用::后的变量来表示
    !integer的意思是声明一块保存整数的内存空间，用::后的变量来表示
    !integer,parameter的意思是声明一块保存数组的内存空间，用::后的变量来表示
    !声明变量用来计算

    !因为Fortran不分大小写而这样命名
    real :: A11, A22, A12, A66, A16, A26 !此处指正则化面内刚度系数
    real :: a11_, a22_, a12_, a66_, a16_, a26_  !此处指正则化面内柔度系数
    real :: V1A,V2A,V3A,V4A       
    real :: Ex, Ey, Gxy !x方向的拉压弹性模量,y方向的拉压弹性模量,剪切弹性模量
    real :: U1Q, U2Q, U3Q, U4Q !计算A11, A22, A12, A66, A16, A26会用到的过程变量
    real :: D11,D22,D12,D16,D26,D66 !此处指正则化弯曲刚度系数
    real :: v1d, v2d, v3d, v4d !弯曲刚度几何因子
    real :: angle0_, angle90_, angle45_, angle_45_, angle_sum_ !对应角度的铺层数和总铺层数
    real :: A                             !用于处理一些过程的过渡变量
    integer :: count                      !用于处理一些过程的过渡变量
    integer :: count4                     !用于处理一些过程的过渡变量
    integer :: b !用于判断
    integer :: i2,i3 !用于循环的计数器
    integer :: temp !用于获得4种不同角度的铺层的数量



    !character的意思是声明一块保存字符串的内存空间，用::后的变量来表示，且该变量="origin.txt"
    !len=20指字符串长度为20
    character(len=20) :: filename= "origin.txt"   
    character(len=20) :: material_code !用于暂时储存用户输入的材料代号
    character(len=15) :: judge !用于记录一种材料的优化目标
    !character(len=1000) :: buf_string !用于暂时储存用户输入的字符串   


    !用于子程序
    integer, parameter :: num_types = 4  !parameter关键字用于定义常量。这些常量在程序运行期间保持不变，不能被修改。
    integer :: layer_orientation(num_types) !储存4种角度中每种角度的板的数量
    integer :: layer_count1,d !用于处理一些过程的过渡变量
    integer :: boards_total  !板的总数
    integer :: board1,board2,board3,board4,fact,jiecheng !计算阶乘时用
    
    integer, allocatable :: permutation(:) !作为参数传递给子程序。储存每一次排列的情况




    !type用来创建一个由编程的人自已定义的数据类型
    !此处是为了方便我用数组储存数据
    !如果用数组储存这个类型的数据，则调用这个数组的一个区域，就会调用这个区域的所有数据


    
    type box3
        character(len=20) ::   order_material
        integer :: Ex_1,Ey_2,Gxy_3,xG_4,yG_5,xy_6,Gy_7  !可以继续添加判断标准
    end type box3

    type(box3),allocatable :: material (:) !material用于记录所有材料的所有优化目标



    type save_layerment
        character(len=20) ::   material_name
        integer :: number_layerment
        integer :: angle_0, angle_45, angle_90, angle_neg_45, angle_sum
    end type save_layerment

    type(save_layerment), allocatable :: jisuan_layerment(:) 


    type save_UQ
        character(len=20) ::   type_material
        real :: uq1,uq2,uq3,uq4
    end type save_UQ

    type(save_UQ), allocatable :: save_U_Q(:)



    type save_VD
        real :: vd1,vd2,vd3,vd4
    end type save_VD

    type(save_VD) ,allocatable :: save_V_D (:) !用于子程序
    







    !b的值用来判断用户的选择
    b=0


    !创建一个all_output.txt文件。如果重新运行程序，就会清空这个all_output.txt文件的所有内容
    open (13, file = "all_output.txt",status = 'replace')
    close(13)
    !记录Ex,Ey,Gxy,Ex/Gxy, Ey/Gxy,Ex/Ey,Gxy/Ey的所有计算结果


    open (14, file = "output.txt",status = 'replace')
    close(14)
    !记录Ex,Ey,Gxy,Ex/Gxy, Ey/Gxy,Ex/Ey,Gxy/Ey的最大值




    open (15, file = "all_D_output.txt",status = 'replace')
    close(15)
    !记录"D11","D12","D16","D22","D26"

    open (16, file = "D_output.txt",status = 'replace')
    close(16)
    !记录"D11","D12","D16","D22","D26"




    


    write(*,*) "请输入需要计算的材料有几种"
    read(*,*)material_total


    !allocate用来设定可变数组可以获得的内存大小

    allocate(save_U_Q(material_total)) !save_U_Q用于记录各个UQ的值
    allocate(jisuan(material_total))  !jisuan用于记录每种材料的数字代号,该材料的E1”, “E2”, “V1”,“G12”
    allocate(material(material_total))


    !用来了解数组需要的大小，用来确定循环次数（即确定计算多少次）
    write(*,*) "请输入铺层设计的方案数"
    read(*,*)layer_total


    allocate(liebiao(layer_total))  !liebiao用于记录所有铺层方式
    allocate(result(layer_total))   !result用于记录Ex,Ey,Gxy,Ex/Gxy, Ey/Gxy,Ex/Ey,Gxy/Ey
    allocate(jisuan_layerment(layer_total)) !jisuan_layerment用于记录每种材料的所有铺层方式

    



    write(*,*)"如果利用文件来输入材料参数,则输入2。如果自己输入所有参数,则输入3"
    write(*,*)"文件内容要求:按照“该材料数字代号”,“该材料的E1”, “E2”, “V1”, “G12”的顺序记录在同一行"
    write(*,*)"文件内容要求:同一材料的不同参数用空格间隔。不同材料在不同行。不用空行。将文件命名为origin.txt"
    write(*,*)"文件内容要求:铺层方式用另一个文件记录。文件命名为Layering_method.txt"
    read(*,*)b
    if (b == 2) then        
        call use_file(filename) !调用子程序。将origin.txt文件中的数据记录在列表中
    else if (b == 3) then
        write(*,*)"总共需要输入",material_total,"个材料"
        write(*,*) "请输入每种材料的数字代号,该材料的E1”, “E2”, “V1”,“G12”。以空格间隔"
        write(*,*) "每输入完一种材料按一下Enter键"

        do i=1,material_total,1
            read(*,*) jisuan(i)%name,jisuan(i)%E1_,jisuan(i)%E2_,jisuan(i)%V1_,jisuan(i)%G12_
        end do


        !只确定各个角度的铺层有几种，不确定具体铺层时用这个
        write(*,*)"请输入需要的铺层方式。一次输入4个数字"
        write(*,*) "每输入完一种铺层方式按一下Enter键"

        do i=1,layer_total,1
            read(*,*)liebiao(i)
        end do

    else
        !明确每一个层合板的角度时用这个
        write(*,*)"总共需要输入",material_total,"个材料"
        write(*,*) "请输入每种材料的数字代号,该材料的E1”, “E2”, “V1”,“G12”。以空格间隔"
        write(*,*) "每输入完一种材料按一下Enter键"

        do i=1,material_total,1
            read(*,*) jisuan(i)%name,jisuan(i)%E1_,jisuan(i)%E2_,jisuan(i)%V1_,jisuan(i)%G12_
        end do

        do i=1,layer_total,1
            !write(*,*)
            !write(*,*)"请输入需要的铺层方式。按从上到下的顺序输入每一个层合板的角度,以空格隔开,开头也需要先按一下空格"
            !read(*,*) buf_string
            !jisuan_layerment(i)%number_layerment=i
            !jisuan_layerment(i)%angle_neg_45 =check_string(buf_string,"-45")
            !jisuan_layerment(i)%angle_45     =check_string(buf_string,"45")
            !jisuan_layerment(i)%angle_90     =check_string(buf_string,"90")
            !jisuan_layerment(i)%angle_0      =check_string(buf_string,"0")

            !jisuan_layerment(i)%angle_sum=jisuan_layerment(i)%angle_neg_45 + jisuan_layerment(i)%angle_45 + jisuan_layerment(i)%angle_90 + jisuan_layerment(i)%angle_0
        
        end do

        

    end if


    write(*,*)
    
    do i=1,material_total,1

        !因为输入/会出现问题，所以用数字代替
        write(*,*)"请选择材料",jisuan(i)%name,"的优化目标"
        write(*,*)"优化为从小到大排列。优化目标有“Ex,Ey,Gxy,Ex/Gxy,Ey/Gxy,Ex/Ey,Gxy/Ey”"
        write(*,*)"优化目标的数字代号分别为1,2,3,4,5,6,7"
        write(*,*)"只输入优化目标的数字代号。选择什么优化目标就输入什么数字"
        write(*,*)"输入所有优化目标后按下Enter键"
        read(*,*)judge
        judge=trim(judge)
        write(*,*)

        material(i)%order_material=jisuan(i)%name !记录i号材料的名称

        if (index(judge,"1") /= 0) then !以Ex为基准进行排序
            material(i)%Ex_1=1
        else
            material(i)%Ex_1=0
        end if

        if (index(judge,"2") /= 0) then
            material(i)%Ey_2=2
        else
            material(i)%Ey_2=0
        end if
        
        if (index(judge,"3") /= 0) then
            material(i)%Gxy_3=3
        else
            material(i)%Gxy_3=0
        end if        
        
        if (index(judge,"4") /= 0) then
            material(i)%xG_4=4
        else
            material(i)%xG_4=0
        end if

        if (index(judge,"5") /= 0) then
            material(i)%yG_5=5
        else
            material(i)%yG_5=0
        end if
        
        if (index(judge,"6") /= 0) then
            material(i)%xy_6=6
        else
            material(i)%xy_6=0
        end if        
        
        if (index(judge,"7") /= 0) then
            material(i)%Gy_7=7
        else
            material(i)%Gy_7=0
        end if

    end do




    do i=1, material_total, 1

        call count_u_Q(jisuan) !调用子程序。计算材料的U1Q,U2Q,U3Q,U4Q的值，并储存在save_U_Q数组中


        do count = 1, layer_total, 1

            temp=liebiao(count)
            !angle_45指以-45角进行铺层的单层板的占比
            !因为此时的铺层设计只有4个角度，且角度与数字对应顺序一致
            !所以可以用这个方法依次计算铺层角的占比
            !fortran的整数的除法会直接抹去小数，不会四舍五入
            !这里的mod的意思是使这个变量temp的数值除以10，然后取余数
            angle_45_ = mod(temp,10)  !获得第四位数字。此处指-45度的铺层角
            temp = temp / 10         !除10之后再取余就可以得到第三位数字了
            angle45_ = mod(temp,10)   !获得第三位数字。此处指45度的铺层角
            temp = temp / 10         !除10之后再取余就可以得到第二位数字了
            angle90_ = mod(temp,10)   !获得第二位数字。此处指90度的铺层角
            angle0_ = temp / 10       !获得第一位数字。此处指0度的铺层角
            angle_sum_ = angle0_ + angle45_ + angle90_ + angle_45_

            !计算这个铺层设计方法的总铺层数
            !某个角度的铺层数除以总铺层数就是这个角度的铺层占比



            V1A=count_V1A(angle_sum_,angle0_,angle90_,angle45_,angle_45_)
            V2A=count_V2A(angle_sum_,angle0_,angle90_,angle45_,angle_45_)
            V3A=count_V3A(angle_sum_,angle0_,angle90_,angle45_,angle_45_)
            V4A=count_V4A(angle_sum_,angle0_,angle90_,angle45_,angle_45_)

            !计算对称层合板的正则化刚度系数    
            A11 = U1Q + U2Q*V1A + U3Q*V2A
            A22 = U1Q - U2Q*V1A + U3Q*V2A
            A12 = U4Q - U3Q*V2A
            A66 = (U1Q - U4Q)/real(2,kind=4) -U3Q*V2A
            A16 = (U2Q*V3A)/real(2,kind=4) + U3Q*V4A
            A26 = (U2Q*V3A)/real(2,kind=4) - U3Q*V4A
            
            !为了计算对称层合板的正则化柔度系数的公式更简洁，先计算分母
            A = A11*A22*A66 + real(2,kind=4)*A12*A26*A16 - A66*A12*A12 - A22*A16*A16 - A11*A26*A26


            !计算对称层合板的正则化柔度系数
            a11_ = (A22*A66 - A26*A26)/A
            a12_ = (A16*A26 - A12*A66)/A
            a16_ = (A12*A26 - A22*A16)/A
            a22_ = (A11*A66 - A16*A16)/A
            a26_ = (A12*A16 - A11*A26)/A
            a66_ = (A11*A22 - A12*A12)/A

            !计算Ex,Ey,Gxy
            Ex = real(1,kind=4)/a11_
            Ey = real(1,kind=4)/a22_
            Gxy = real(1,kind=4)/a66_




            !通过循环将每一个铺层方式及这个铺层方式对应的Ex,Ey,Gxy存入数组中
            result(count)%layer= liebiao(count)
            result(count)%Ex_=Ex
            result(count)%Ey_=Ey
            result(count)%Gxy_=Gxy
            result(count)%x_G=Ex/Gxy
            result(count)%y_G=Ey/Gxy
            result(count)%x_y=Ex/EY
            result(count)%G_y=Gxy/Ey
            
        end do


        !根据优化目标，在文件all_output.txt上输出对应的字符串

 
        if (material(i)%Ex_1 == 1) then     !确定以什么为基准进行排序
 
            call sort_results(1) ! 调用通用排序，参数为 1
            
            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Ex"            
            call output_file()
                    
        end if


        if (material(i)%Ey_2 == 2) then
            call sort_results(2) ! 调用通用排序，参数为 2

            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')            
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Ey"
            call output_file()
        
        end if


        if (material(i)%Gxy_3 == 3) then
            call sort_results(3) ! 调用通用排序，参数为 3
            
            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')            
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Gxy"
            call output_file()

        end if


        if (material(i)%xG_4 == 4) then
            call sort_results(4) ! 调用通用排序，参数为 4

            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Ex/Gxy"
            call output_file()

        end if


        if (material(i)%yG_5 == 5) then
            call sort_results(5) ! 调用通用排序，参数为 5

            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Ey/Gxy"
            call output_file()

        end if


        if (material(i)%xy_6 == 6) then
            call sort_results(6) ! 调用通用排序，参数为 6

            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')            
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Ex/Ey"
            call output_file()

        end if


        if (material(i)%Gy_7 == 7) then
            call sort_results(7) ! 调用通用排序，参数为 7

            !打开all_output.txt文件。append指不清除文件内容，如果文件里已经有内容，就在这个内容后继续添加
            !old指文件已经存在
            open (13, file = "all_output.txt",status = 'old',position='append')            
            write(13,*)
            write(13,*) "材料", jisuan(i)%name,"优化目标(从小到大):Gxy/Ey"
            call output_file()

        end if
                 

    end do

    
    !用来提醒用户程序已经完成运行
    write(*,*)"对数据分析和排序完成"  
    write(*,*)"请到all_output.txt文件中进行查看"
    !write表示显示""中所有字符串，第一个*表示输出的位置使用默认值，第二个*表示输出格式为默认格式
    !""和''效果相同


    b=0
    write(*,*)"如果要计算弯曲刚度,则输入1,否则输入其他数字。"
    read(*,*)b



    !为“利用循环来将数值储存到列表中”提供初始值
    d=0
    layer_count1=1

    
    do while (b == 1) 
        write(*,*)"选择需要计算弯曲刚度的材料的代号(之前输入的数字代号)"

        read(*,*) material_code
        
        write(*, *)
        write(*, *) "注意,板的总数大于12时,计算需要很长时间"
        write(*, *) "请按-45,45,90,0的顺序输入板的数量。每输入一个数就按一次回车"        
        do i = 1, 4
            read(*, *) layer_orientation(i)

        end do
        

        !以下的目的是计算排列的总数,总数的阶乘除以(每一种板的数量的阶乘的和)
        board1=layer_orientation(1)

        board2=layer_orientation(2)

        board3=layer_orientation(3)
        board4=layer_orientation(4)
        boards_total=sum(layer_orientation)

        fact=1

        do i=1,board1
            fact=fact*i
        end do


        board1=fact
        fact=1

        do i=1,board2
            fact=fact*i
        end do

            board2=fact
            fact=1

        do i=1,board3
                fact=fact*i
        end do

        board3=fact
        fact=1

        do i=1,board4
            fact=fact*i
        end do  

        board4=fact
        fact=1

        do i=1,boards_total
            fact=fact*i
        end do

        jiecheng=fact/(board1*board2*board3*board4)
        !以上的目的是计算排列的总数



        allocate(save_V_D(jiecheng))
        allocate(layer_arrangements(jiecheng*boards_total))
        allocate(permutation(boards_total))

        !调用子程序
        call generate(1, boards_total, layer_orientation, permutation)



        do i=1,material_total
            if (material_code == save_U_Q(i)%type_material) then !只选择对应的那1个材料的UQ
                do i2=1, jiecheng
                    D11=save_U_Q(i)%uq1 + save_V_D(i2)%vd1 * save_U_Q(i)%uq2 + save_V_D(i2)%vd2 * save_U_Q(i)%uq3
                    D12=save_U_Q(i)%uq4 - save_V_D(i2)%vd2 * save_U_Q(i)%uq3
                    D16=(save_V_D(i2)%vd3) /2 + save_V_D(i2)%vd4 * save_U_Q(i)%uq3
                    D22=save_U_Q(i)%uq1 - save_V_D(i2)%vd1 * save_U_Q(i)%uq2 + save_V_D(i2)%vd2 * save_U_Q(i)%uq3
                    D26=(save_V_D(i2)%vd3) /2 - save_V_D(i2)%vd4 * save_U_Q(i)%uq3
                    D66=(save_U_Q(i)%uq1 - save_U_Q(i)%uq4) /2 - save_V_D(i2)%vd2 * save_U_Q(i)%uq3

                    open (15, file = "all_D_output.txt",status = 'old',position='append')
                    write(15,"(6X,A12,$)")"材料代号","D11","D12","D16","D22","D26","D66"
                    write(15,*)
                    write(15,*)material_code," : ",D11,D12,D16,D22,D26,D66
                    write(15,*)"排列方式"
                    do i3=1,boards_total
                        write(15,"(I4)",advance='no')layer_arrangements((i2-1)*boards_total+i3)
                        !因为储存排列方式的时候，每一种排列方式都要储存boards_total个数据
                        !I4指以4个字符宽来输出整数，advance='no'指不换行输出
                    end do
                    write(15,*)
                    write(15,*)
                    close(15)
                                        

                end do    



                exit !用来结束这层循环

            end if
        end do

        write(*,*)"如果要继续计算弯曲刚度,则输入1,否则输入其他数字。"
        read(*,*)b

        deallocate(save_V_D) !重置数组，以此进行下次储存
        deallocate(layer_arrangements)
        deallocate(permutation)

        d = 0 !重置因为之前计算而改变的变量，如果要重新计算，就不会出现问题
        layer_count1 = 1



    end do


contains
 


!使用 CONTAINS 的最大好处是，内部子程序可以直接访问主程序中声明的变量、参数和类型，而无需将它们作为参数传递。
!intent(in)：表示参数是输入参数，只能读取，不允许在子程序或函数中修改其值。
!intent(inout)：表示参数既可以作为输入，也可以在子程序或函数中修改并作为输出返回。
!intent(out)：表示参数是输出参数，子程序或函数中必须为其赋值，进入子程序时其初始值会被忽略。
!recursive 关键字用于声明 generate 子程序是一个递归过程。这意味着该子程序可以直接或间接地调用自身



    ! 递归子程序来生成排列,并计算所有可能的排列的对称层合板的弯曲刚度几何因子
    recursive subroutine generate(level, total_use, current_counts, current_perm)
        use global
        implicit none
        integer, intent(in) :: level, total_use
        integer, intent(inout) :: current_counts(num_types)
        integer, intent(inout) :: current_perm(total_use) !总共有几块板，1个排列就有几个块板需要被使用
        integer :: j,k
        real :: r,middle1,middle2,middle3,middle4
        integer :: chars(num_types) = [integer :: -45, 45, 90, 0]


        !每次排列1块板，当排完所有板时，即level的值大于板的总数时,就开始计算。然后再
        if (level > total_use) then

            !因为每一种排列方式都要算，所以算完一次就要把数据重新变为0
            v1d=0.0
            v2d=0.0
            v3d=0.0
            v4d=0.0


            do k=1, total_use

                !弯曲刚度几何因子的计算
                r=current_perm(k)

                middle1=(3*k*k-3*k+1)*cosd(2*r)
                middle2=(3*k*k-3*k+1)*cosd(4*r)
                middle3=(3*k*k-3*k+1)*sind(2*r)
                middle4=(3*k*k-3*k+1)*sind(4*r)

                v1d=middle1+v1d
                v2d=middle2+v2d
                v3d=middle3+v3d
                v4d=middle4+v4d



                !用来储存每一种排列方式
                layer_arrangements(layer_count1)=current_perm(k)
                layer_count1=layer_count1+1

            end do


            !每个排列的4个弯曲刚度几何因子储存在1个save_V_D数组的1个位置上
            d=d+1           
            
            !储存
            save_V_D (d)%vd1=v1d
            save_V_D (d)%vd2=v2d
            save_V_D (d)%vd3=v3d
            save_V_D (d)%vd4=v4d


            return !执行该命令也会结束当前的子程序

        end if


        ! 递归步骤：尝试将每种可用的板放在当前位置
        do j = 1, num_types
            if (current_counts(j) > 0) then
                ! “选择”一个板
                current_counts(j) = current_counts(j) - 1
                current_perm(level) = chars(j)

                ! 递归到下一个位置
                call generate(level + 1, total_use, current_counts, current_perm)

                ! “撤销选择”（回溯），以便在下一次循环中尝试其他板
                current_counts(j) = current_counts(j) + 1
            end if
        end do
    end subroutine generate !执行该命令也会结束当前的子程序




    !sort_results用来排序

    !仅对数组result的相关数值进行排序，但每一个铺层方式对应的所有数值会同时移动（因为box2）
    !首先确定以什么为基准进行排序
    !然后通过循环，先选出数组中对应基准的所有数据中数值最小的那一个，排到第一位
    !然后排除第一位的那一个数据，从剩下的数据中选取最小的那一个，排到第二位
    !因此类推，直到所有数据的顺序由小到大排列好
        
    !layer_total-1的原因：总共layer_total个数据,只需要将数据确定在layer_total-1个位置上,就能让第layer_total个位置的数据是最大的
    !如果count1=layer_total,则count2=layer_total+1,此时下方的循环因为layer_total+1>layer_total而不会发生。



    !仅对数组result的相关数值进行排序，但每一个铺层方式对应的所有数值会同时移动（因为box2）
        !通过循环，先选出数组中对应基准的所有数据中数值最小的那一个，排到第一位
        !然后排除第一位的那一个数据，从剩下的数据中选取最小的那一个，排到第二位
        !因此类推，直到所有数据的顺序由小到大排列好
            
        !layer_total-1的原因：总共layer_total个数据,只需要将数据确定在layer_total-1个位置上,就能让第layer_total个位置的数据是最大的
        !如果count1=layer_total,则count2=layer_total+1,此时下方的循环因为layer_total+1>layer_total而不会发生。

    subroutine sort_results(sort_criterion)
        use global
        implicit none
        integer, intent(in) :: sort_criterion ! 输入参数：1=Ex, 2=Ey, 3=Gxy, 等

        ! 这是一个经典的选择排序算法。
        ! 外层循环确定已排序部分的末尾位置。
        do count1 = 1, layer_total - 1
            ! 内层循环在未排序部分中查找最小的元素。
            do count2 = count1 + 1, layer_total
                
                ! 使用 select case 来决定以什么作为比较依据
                select case (sort_criterion)

                    case (1) ! 按 Ex 排序
                        if (result(count1)%Ex_ > result(count2)%Ex_) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                    case (2) ! 按 Ey 排序
                        if (result(count1)%Ey_ > result(count2)%Ey_) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                    case (3) ! 按 Gxy 排序
                        if (result(count1)%Gxy_ > result(count2)%Gxy_) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                    case (4) ! 按 Ex/Gxy 排序
                        if (result(count1)%x_G > result(count2)%x_G) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                    case (5) ! 按 Ey/Gxy 排序
                        if (result(count1)%y_G > result(count2)%y_G) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                    case (6) ! 按 Ex/Ey 排序
                        if (result(count1)%x_y > result(count2)%x_y) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                    case (7) ! 按 Gxy/Ey 排序
                        if (result(count1)%G_y > result(count2)%G_y) then
                            count3 = result(count1)
                            result(count1) = result(count2)
                            result(count2) = count3
                        end if

                        !如有需要，可以添加更多的排序标准

                end select

            end do
        end do
    end subroutine sort_results    



    !用来将数据记录到文件中
    subroutine output_file()
        use global
        implicit none
        !6X把输出的位置向右跳过6个位置，A6指以6个字符宽来输出字符串，$指不换行输出
        write(13,"(6X,A9,$)")"铺层","Ex/GPa","Ey/Gpa","  Gxy/GPa","  Ex/Gxy", "  Ey/Gxy","  Ex/Ey","   Gxy/Ey"
        write(13,*)
                

        do count4=1,layer_total
                
            write(13,*)    !用来输出空行
            write(13,*)result(count4)%layer,result(count4)%Ex_, result(count4)%Ey_, result(count4)%Gxy_ ,result(count4)%x_G ,result(count4)%y_G, result(count4)%x_y, result(count4)%G_y
            !按顺序输出数组result中的储存的信息,铺层方式,Ex,Ey,Gxy等数据的对应值 

            write(13,*)
        end do

        close(13)
    end subroutine output_file





    !此处用来将文件origin.txt中的数据记录在列表jisuan中
    subroutine use_file(file_name)
        use global
        use material_parameters_save
        implicit none
        character(len=20), intent(in) :: file_name
        logical alive !声明逻辑变量
        integer,parameter :: fileid =11
        integer :: error,stat    

        !用于判断文件是否存在
        !查询origin.txt文件是否存在。如果不存在，会把alive的值设为.False.
        inquire(file=file_name,exist=alive) 
        if(.not.alive)then  !如果alive的值为.False. ，则执行
            write(*,*)file_name,"不存在"
            stop
        end if

        inquire(file="Layering_method.txt",exist=alive) 
        if(.not.alive)then  !如果alive的值为.False. ，则执行
            write(*,*)"Layering_method.txt","不存在"
            stop
        end if        



        !open用来打开文件
        !此处用来获得origin.txt中的数据，并将其记录在列表jisuan中
        open (fileid,file = "origin.txt") !打开文件origin.txt,将其命名为fileid,该值为11

        do i=1 , material_total , 1

            !此处的read表示将origin.txt这个文件中的数据记录在数组jisuan中

            read(fileid,*,IOSTAT=error) jisuan(i)%name, jisuan(i)%E1_, jisuan(i)%E2_, jisuan(i)%V1_, jisuan(i)%G12_ 
            !此处的error无须赋值，程序在读取到文件最后会自动更改error的值。变量名无要求

            if (error /= 0) exit !如果已经读取到文件最后的内容，就停止读取，不停止会出现错误
        end do !表示用于循环的代码到此为止

        close(11) !关闭打开的origin.txt文件



        !此处用来得到文件Layering_method.txt中的数据，并将其记录在列表liebiao中
        open (12,file = "Layering_method.txt") !该文件储存铺层设计方式

        !1的意思是计数器的增值为1（可为其他数值.如果不写出来就默认为1）
        !g在此处作为循环终止的数值，当计数器的值大于g的值时，循环结束
        !i=1中的i是循环的计数器，此处的1为i的值。每执行一次循环，就从这个值开始增加，此时增值为1
        do i=1 , layer_total , 1

            !12表示给Layering_method.txt文件一个代号，之后用这个代号就表示对这个文件进行处理
            !IOSTAT用来确认文件的读写状态，stat=0表示正常，其他情况表示不正常
            !将*表示其他设置均为默认设置
            read (12,*,IOSTAT=stat) liebiao !将Layering_method.txt里的数据一个一个地加到数组中

            if (stat /= 0) exit !如果已经读取到文件最后的内容，就停止读取，不停止会出现错误
        end do !表示用于循环的代码到此为止

        close(12) !关闭打开的Layering_method.txt文件        

        return
    end subroutine use_file

    
    subroutine count_u_Q(suan)
        use global

        implicit none
        real :: E1, E2, G12, V1, V2  !5个工程弹性常数
        real :: M  !中间变量
        real :: Q11, Q12, Q21, Q22, Q66 !模量分量, 因为是对称层合板，Q16=Q61=Q26=Q62=0

        type(box) ,allocatable :: suan (:) !记录材料的4个工程弹性常数


        !按顺序依次读取已经储存在数组中的值

        !%的意思是选择自定义数据中的某一部分
        !E1=jisuan(i)%E1_ 指选择jisuan(i)中的5个变量中的变量E1_并将E1_的值赋给E1
        E1=suan(i)%E1_
        E2=suan(i)%E2_
        V1=suan(i)%V1_
        G12=suan(i)%G12_



        V2 = (V1 * E2)/E1  !因为V2未知，所以用此公式求出V2
        M = real(1,kind=4)/(real(1,kind=4)-V1 * V2)  !因为M未知，所以用此公式求出M

        !以模量分量的定义式求出Q11, Q12, Q21, Q22, Q66
        Q11 = M*E1
        Q12 = M*V2*E1
        Q21 = M*V1*E2
        Q22 = M*E2
        Q66 = G12


        !用来计算对称层合板的正则化刚度系数的需要的变量
        U1Q = (real(3,kind=4)*Q11 + real(3,kind=4)*Q22 + real(2,kind=4)*Q12 + real(4,kind=4)*Q66)/8
        U2Q = (Q11 - Q22)/real(2,kind=4)
        U3Q = (Q11 + Q22 - real(2,kind=4)*Q12 - real(4,kind=4)*Q66)/real(8,kind=4)
        U4Q = (Q11 + Q22 + real(6,kind=4)*Q12 - real(4,kind=4)*Q66)/real(8,kind=4)


        !储存，为之后计算对称层合板的弯曲刚度几何因子做准备
        save_U_Q(i)%type_material=jisuan(i)%name
        save_U_Q(i)%uq1=U1Q
        save_U_Q(i)%uq2=U2Q
        save_U_Q(i)%uq3=U3Q
        save_U_Q(i)%uq4=U4Q

    end subroutine count_u_Q



    real function count_V1A(anglesum,angle0,angle90,angle45,angle_45)
        implicit none

        real, intent(in) :: anglesum,angle0,angle90,angle45,angle_45



        !计算层合板面内刚度正则化的几何因子
        

        !0度
        !因为0*2=0，0*4=0, 所以直接算cos0
        !加上real是为了确保括号内的数值为浮点数
        count_V1A = angle0/anglesum * cosd(real(0)) !cosd指对括号里的内容(视为角度)，进行cos计算
        

        !90
        count_V1A = angle90/anglesum * cosd(real(90 * 2)) + count_V1A   !这里加上之前已经算好的V1A来实现求和

        !-45
        count_V1A = angle_45/anglesum * cosd(real(-45 * 2)) + count_V1A

        !45
        count_V1A = angle45/anglesum * cosd(real(45 * 2)) + count_V1A
       

    end function count_V1A

    real function count_V2A(anglesum,angle0,angle90,angle45,angle_45)
        implicit none

        real, intent(in) :: anglesum,angle0,angle90,angle45,angle_45

        !计算层合板面内刚度正则化的几何因子

        !0度
        count_V2A = angle0/anglesum * cosd(real(0)) !cosd指对括号里的内容(视为角度)，进行cos计算

        !90
        count_V2A = angle90/anglesum * cosd(real(90 * 4)) + count_V2A   !这里加上之前已经算好的V2A来实现求和

        !-45
        count_V2A = angle_45/anglesum * cosd(real(-45 * 4)) + count_V2A

        !45
        count_V2A = angle45/anglesum * cosd(real(45 * 4)) + count_V2A
       

    end function count_V2A

    real function count_V3A(anglesum,angle0,angle90,angle45,angle_45)
        implicit none

        real, intent(in) :: anglesum,angle0,angle90,angle45,angle_45

        !计算层合板面内刚度正则化的几何因子

        !0度
        count_V3A = angle0/anglesum * sind(real(0)) !sind指对括号里的内容(视为角度)，进行sin计算

        !90
        count_V3A = angle90/anglesum * sind(real(90 * 2)) + count_V3A   !这里加上之前已经算好的V3A来实现求和

        !-45
        count_V3A = angle_45/anglesum * sind(real(-45 * 2)) + count_V3A

        !45
        count_V3A = angle45/anglesum * sind(real(45 * 2)) + count_V3A
       

    end function count_V3A

    real function count_V4A(anglesum,angle0,angle90,angle45,angle_45)
        implicit none

        real, intent(in) :: anglesum,angle0,angle90,angle45,angle_45

        !计算层合板面内刚度正则化的几何因子

        !0度
        count_V4A = angle0/anglesum * sind(real(0)) !sind指对括号里的内容(视为角度)，进行sin计算

        !90
        count_V4A = angle90/anglesum * sind(real(90 * 4)) + count_V4A   !这里加上之前已经算好的V4A来实现求和

        !-45
        count_V4A = angle_45/anglesum * sind(real(-45 * 4)) + count_V4A

        !45
        count_V4A = angle45/anglesum * sind(real(45 * 4)) + count_V4A
       
        return

    end function count_V4A


    !搜索目标字符串在某个长字符串中出现多少次。返回值为出现的次数
    integer function check_string(buf,aim_string)
        implicit none
        character(len=1000) :: buf
        character(len=:), allocatable :: origin_string
        character(len=10) :: aim_string
        integer :: pos, p, len_str


        origin_string = trim(buf)


        aim_string =  trim(aim_string)

        len_str = len(aim_string)

        pos = 1
        check_string = 0

        do while (pos <= len(origin_string))
            p = index(origin_string(pos:), aim_string)    ! 先查找origin_string中第一个目标字符串
            if (p == 0) exit
            check_string = check_string + 1
            pos = pos + p + len_str - 1   ! 跳过刚找到的目标字符串，继续查找后面的部分
        end do

    end function check_string





end program calculation_Composites  !用来封装程序代码，说明主程序代码已经编写完毕    