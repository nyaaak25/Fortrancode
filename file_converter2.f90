! pythonで作成した吸収係数を入れて、binary fileに変換するプログラム
! @author: A.Kazama kazama@pparc.gp.tohoku.ac.jp


! ver. 1.0: binary：　pythonのfileを読み込んで、 .v fileを作成する
! Created on Sun Apr 10 16:10:00 2022

! ver. 1.1: binary:  pythonのfileを読み込んで、 .atmos file, .k fileを作成する
! Created on Wed Apr 13 10:19:00 2022


program Converter
    implicit none  ! おまじないで絶対に必要なもの

    ! parameter指定
    character (LEN=1000) :: Openfile01,Openfile02,Openfile03,OPFIL01,OPFIL02,OPFIL03
    character (LEN=1000) :: Openfile04,Openfile05,Openfile06,OPFIL04,OPFIL05,OPFIL06
    character (LEN=1000) :: Openfile07,Openfile08,Openfile09,OPFIL07,OPFIL08,OPFIL09
    character (LEN=1000) :: Openfile10,Openfile11,Openfile12,OPFIL10,OPFIL11,OPFIL12
    character (LEN=1000) :: Openfile13,Openfile14,Openfile15,OPFIL13,OPFIL14,OPFIL15
    character (LEN=1000) :: Openfile16,Openfile17,Openfile18,OPFIL16,OPFIL17,OPFIL18
    character (LEN=1000) :: Openfile19,Openfile20,Openfile21,OPFIL19,OPFIL20,OPFIL21
    character (LEN=1000) :: Openfile22,Openfile23,Openfile24,OPFIL22,OPFIL23,OPFIL24
    character (LEN=1000) :: Openfile25,Openfile26,Openfile27,OPFIL25,OPFIL26,OPFIL27
    character (LEN=1000) :: Openfile28,Openfile29,Openfile30,OPFIL28,OPFIL29,OPFIL30
    character (LEN=1000) :: Openfile31,OPFIL31 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i,j,k,line_counter1, line_counter2 ! ALLOCATABLEはあとで確保してあげようとしている。確保しないと使えない。
    double precision,allocatable::WN(:)
    double precision,allocatable::Kw1(:,:),Hight1(:),Temp1(:),Press1(:)
    double precision,allocatable::Kw2(:,:),Hight2(:),Temp2(:),Press2(:)
    double precision,allocatable::Kw3(:,:),Hight3(:),Temp3(:),Press3(:)
    double precision,allocatable::Kw4(:,:),Hight4(:),Temp4(:),Press4(:)
    double precision,allocatable::Kw5(:,:),Hight5(:),Temp5(:),Press5(:)
    double precision,allocatable::Kw6(:,:),Hight6(:),Temp6(:),Press6(:)
    double precision,allocatable::Kw7(:,:),Hight7(:),Temp7(:),Press7(:)
    double precision,allocatable::Kw8(:,:),Hight8(:),Temp8(:),Press8(:)
    double precision,allocatable::Kw9(:,:),Hight9(:),Temp9(:),Press9(:)
    double precision,allocatable::Kw10(:,:),Hight10(:),Temp10(:),Press10(:)
    double precision,allocatable::Kw11(:,:),Hight11(:),Temp11(:),Press11(:)
    double precision,allocatable::Kw12(:,:),Hight12(:),Temp12(:),Press12(:)
    double precision,allocatable::Kw13(:,:),Hight13(:),Temp13(:),Press13(:)
    double precision,allocatable::Kw14(:,:),Hight14(:),Temp14(:),Press14(:)
    double precision,allocatable::Kw15(:,:),Hight15(:),Temp15(:),Press15(:)



    ! --------------------------- .v file makeing binaryfile ---------------------------
    ! Input file
    Openfile01 = '/Users/nyonn/Desktop/pythoncode/ret_dust_2_tau.txt'
    OPEN(1, FILE = Openfile01,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter1 = 0
    do
        READ(1,*,end = 999)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter1 = line_counter1+1  ! 列をわかるようにしている
    enddo
    999 continue
    rewind(1)
    allocate(WN(line_counter1))

    DO i=1,line_counter1
        READ(1,*) WN(i)
    ENDDO

    CLOSE(1)

    ! Output File
    OPFIL01 = 'dust_LUT.v'
    OPEN (2,FILE=OPFIL01,FORM='UNFORMATTED',access='stream',STATUS='replace')
    WRITE(2) 1, 1
    WRITE(2) 1, line_counter1
    WRITE(2) WN
    CLOSE(2)

    ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile02 = '/Users/nyonn/Desktop/pythoncode/ret_dust_2.txt'
    OPEN(3, FILE = Openfile02,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(3,*,end = 99)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    99 continue
    rewind(3)
    allocate(Hight1(line_counter2),Press1(line_counter2),Temp1(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(3,*) Hight1(i),Press1(i),Temp1(i)
    ENDDO

    CLOSE(3)

    ! Output file
    OPFIL02 = 'ret_dust_2.atmos'
    OPEN (4,FILE=OPFIL02,FORM='FORMATTED',STATUS='replace')
    WRITE(4,*) line_counter2
    DO i = 1, line_counter2
        WRITE(4,*) Hight1(i), Temp1(i), Press1(i)/100.
    enddo
    CLOSE(4)


    ! --------------------------- .k file makeing  binaryfile ---------------------------
    ! Input file
    Openfile31 = '/Users/nyonn/Desktop/pythoncode/ret_dust_2_Kw.txt'
    OPEN(61, FILE = Openfile31,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    allocate(Kw15(29500,31))  ! メモリの確保ができたら、そのファイルを読み込む
    DO j=1,29500
        READ(61,*) (Kw15(j,k),k=1,31)
    ENDDO
    CLOSE(61)

    ! Output File
    OPFIL31 = 'ret_dust_2.k'
    OPEN (62,FILE=OPFIL31,FORM='UNFORMATTED',access='stream',STATUS='replace')
    DO i = line_counter2, 1, -1
        WRITE(62) 1, real(Hight1(i)), real(Temp1(i)), real(Press1(i)/100.), real(Kw15(:,i)*1E5)
    ENDDO
    CLOSE(62)


end program Converter
