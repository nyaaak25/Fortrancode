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
    ! character (LEN=1000) :: Openfile19,Openfile20,Openfile21,OPFIL19,OPFIL20,OPFIL21
    ! character (LEN=1000) :: Openfile22,Openfile23,Openfile24,OPFIL22,OPFIL23,OPFIL24
    ! character (LEN=1000) :: Openfile25,Openfile26,Openfile27,OPFIL25,OPFIL26,OPFIL27
    ! character (LEN=1000) :: Openfile28,Openfile29,Openfile30,OPFIL28,OPFIL29,OPFIL30
    ! character (LEN=1000) :: Openfile31,Openfile32,Openfile33,OPFIL31,OPFIL32,OPFIL33
    ! character (LEN=1000) :: Openfile34,Openfile35,Openfile36,OPFIL34,OPFIL35,OPFIL36 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i,j,k,line_counter1, line_counter2 ! ALLOCATABLEはあとで確保してあげようとしている。確保しないと使えない。
    double precision,allocatable::WN(:),Kw(:,:),Hight(:),Temp(:),Press(:)
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
    Openfile01 = '/Users/nyonn/Desktop/pythoncode/4545-5556_0.01step_cutoff_120.txt'
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
    OPFIL01 = 'vfile/LUtable.v'
    OPEN (2,FILE=OPFIL01,FORM='UNFORMATTED',access='stream',STATUS='replace')
    WRITE(2) 1, 1
    WRITE(2) 1, line_counter1
    WRITE(2) WN
    CLOSE(2)








    ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile02 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS50.txt'
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
    OPFIL02 = 'atmosfile/LUTable_T1_285_T2_200_PRS50.atmos'
    OPEN (4,FILE=OPFIL02,FORM='FORMATTED',STATUS='NEW')
    WRITE(4,*) line_counter2
    DO i = 1, line_counter2
        WRITE(4,*) Hight1(i), Temp1(i), Press1(i)/100.
    enddo
    CLOSE(4)


        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile03 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS150.txt'
    OPEN(5, FILE = Openfile03,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(5,*,end = 100)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    100 continue
    rewind(5)
    allocate(Hight2(line_counter2),Press2(line_counter2),Temp2(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(5,*) Hight2(i),Press2(i),Temp2(i)
    ENDDO

    CLOSE(5)

    ! Output file
    OPFIL03 = 'atmosfile/LUTable_T1_285_T2_200_PRS150.atmos'
    OPEN (6,FILE=OPFIL03,FORM='FORMATTED',STATUS='NEW')
    WRITE(6,*) line_counter2
    DO i = 1, line_counter2
        WRITE(6,*) Hight2(i), Temp2(i), Press2(i)/100.
    enddo
    CLOSE(6)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile04 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS180.txt'
    OPEN(7, FILE = Openfile04,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(7,*,end = 101)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    101 continue
    rewind(7)
    allocate(Hight3(line_counter2),Press3(line_counter2),Temp3(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(7,*) Hight3(i),Press3(i),Temp3(i)
    ENDDO

    CLOSE(7)

    ! Output file
    OPFIL04 = 'atmosfile/LUTable_T1_285_T2_200_PRS180.atmos'
    OPEN (8,FILE=OPFIL04,FORM='FORMATTED',STATUS='NEW')
    WRITE(8,*) line_counter2
    DO i = 1, line_counter2
        WRITE(8,*) Hight3(i), Temp3(i), Press3(i)/100.
    enddo
    CLOSE(8)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile05 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS215.txt'
    OPEN(9, FILE = Openfile05,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(9,*,end = 102)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    102 continue
    rewind(9)
    allocate(Hight4(line_counter2),Press4(line_counter2),Temp4(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(9,*) Hight4(i),Press4(i),Temp4(i)
    ENDDO

    CLOSE(9)

    ! Output file
    OPFIL05 = 'atmosfile/LUTable_T1_285_T2_200_PRS215.atmos'
    OPEN (10,FILE=OPFIL05,FORM='FORMATTED',STATUS='NEW')
    WRITE(10,*) line_counter2
    DO i = 1, line_counter2
        WRITE(10,*) Hight4(i), Temp4(i), Press4(i)/100.
    enddo
    CLOSE(10)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile06 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS257.txt'
    OPEN(11, FILE = Openfile06,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(11,*,end = 103)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    103 continue
    rewind(11)
    allocate(Hight5(line_counter2),Press5(line_counter2),Temp5(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(11,*) Hight5(i),Press5(i),Temp5(i)
    ENDDO

    CLOSE(11)

    ! Output file
    OPFIL06 = 'atmosfile/LUTable_T1_285_T2_200_PRS257.atmos'
    OPEN (12,FILE=OPFIL06,FORM='FORMATTED',STATUS='NEW')
    WRITE(12,*) line_counter2
    DO i = 1, line_counter2
        WRITE(12,*) Hight5(i), Temp5(i), Press5(i)/100.
    enddo
    CLOSE(12)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile07 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS308.txt'
    OPEN(13, FILE = Openfile07,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(13,*,end = 104)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    104 continue
    rewind(13)
    allocate(Hight6(line_counter2),Press6(line_counter2),Temp6(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(13,*) Hight6(i),Press6(i),Temp6(i)
    ENDDO

    CLOSE(13)

    ! Output file
    OPFIL07 = 'atmosfile/LUTable_T1_285_T2_200_PRS308.atmos'
    OPEN (14,FILE=OPFIL07,FORM='FORMATTED',STATUS='NEW')
    WRITE(14,*) line_counter2
    DO i = 1, line_counter2
        WRITE(14,*) Hight6(i), Temp6(i), Press6(i)/100.
    enddo
    CLOSE(14)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile08 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS369.txt'
    OPEN(15, FILE = Openfile08,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(15,*,end = 105)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    105 continue
    rewind(15)
    allocate(Hight7(line_counter2),Press7(line_counter2),Temp7(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(15,*) Hight7(i),Press7(i),Temp7(i)
    ENDDO

    CLOSE(15)

    ! Output file
    OPFIL08 = 'atmosfile/LUTable_T1_285_T2_200_PRS369.atmos'
    OPEN (16,FILE=OPFIL08,FORM='FORMATTED',STATUS='NEW')
    WRITE(16,*) line_counter2
    DO i = 1, line_counter2
        WRITE(16,*) Hight7(i), Temp7(i), Press7(i)/100.
    enddo
    CLOSE(16)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile09 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS442.txt'
    OPEN(17, FILE = Openfile09,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(17,*,end = 106)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    106 continue
    rewind(17)
    allocate(Hight8(line_counter2),Press8(line_counter2),Temp8(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(17,*) Hight8(i),Press8(i),Temp8(i)
    ENDDO

    CLOSE(17)

    ! Output file
    OPFIL09 = 'atmosfile/LUTable_T1_285_T2_200_PRS442.atmos'
    OPEN (18,FILE=OPFIL09,FORM='FORMATTED',STATUS='NEW')
    WRITE(18,*) line_counter2
    DO i = 1, line_counter2
        WRITE(18,*) Hight8(i), Temp8(i), Press8(i)/100.
    enddo
    CLOSE(18)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile10 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS529.txt'
    OPEN(19, FILE = Openfile10,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(19,*,end = 107)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    107 continue
    rewind(19)
    allocate(Hight9(line_counter2),Press9(line_counter2),Temp9(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(19,*) Hight9(i),Press9(i),Temp9(i)
    ENDDO

    CLOSE(19)

    ! Output file
    OPFIL10 = 'atmosfile/LUTable_T1_285_T2_200_PRS529.atmos'
    OPEN (20,FILE=OPFIL10,FORM='FORMATTED',STATUS='NEW')
    WRITE(20,*) line_counter2
    DO i = 1, line_counter2
        WRITE(20,*) Hight9(i), Temp9(i), Press9(i)/100.
    enddo
    CLOSE(20)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile11 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS633.txt'
    OPEN(21, FILE = Openfile11,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(21,*,end = 108)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    108 continue
    rewind(21)
    allocate(Hight10(line_counter2),Press10(line_counter2),Temp10(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(21,*) Hight10(i),Press10(i),Temp10(i)
    ENDDO

    CLOSE(21)

    ! Output file
    OPFIL11 = 'atmosfile/LUTable_T1_285_T2_200_PRS633.atmos'
    OPEN (22,FILE=OPFIL11,FORM='FORMATTED',STATUS='NEW')
    WRITE(22,*) line_counter2
    DO i = 1, line_counter2
        WRITE(22,*) Hight10(i), Temp10(i), Press10(i)/100.
    enddo
    CLOSE(22)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile12 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS758.txt'
    OPEN(23, FILE = Openfile12,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(23,*,end = 109)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    109 continue
    rewind(23)
    allocate(Hight11(line_counter2),Press11(line_counter2),Temp11(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(23,*) Hight11(i),Press11(i),Temp11(i)
    ENDDO

    CLOSE(23)

    ! Output file
    OPFIL12 = 'atmosfile/LUTable_T1_285_T2_200_PRS758.atmos'
    OPEN (24,FILE=OPFIL12,FORM='FORMATTED',STATUS='NEW')
    WRITE(24,*) line_counter2
    DO i = 1, line_counter2
        WRITE(24,*) Hight11(i), Temp11(i), Press11(i)/100.
    enddo
    CLOSE(24)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile13 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS907.txt'
    OPEN(25, FILE = Openfile13,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(25,*,end = 110)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    110 continue
    rewind(25)
    allocate(Hight12(line_counter2),Press12(line_counter2),Temp12(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(25,*) Hight12(i),Press12(i),Temp12(i)
    ENDDO

    CLOSE(25)

    ! Output file
    OPFIL13 = 'atmosfile/LUTable_T1_285_T2_200_PRS907.atmos'
    OPEN (26,FILE=OPFIL13,FORM='FORMATTED',STATUS='NEW')
    WRITE(26,*) line_counter2
    DO i = 1, line_counter2
        WRITE(26,*) Hight12(i), Temp12(i), Press12(i)/100.
    enddo
    CLOSE(26)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile14 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS1096.txt'
    OPEN(27, FILE = Openfile14,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(27,*,end = 111)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    111 continue
    rewind(27)
    allocate(Hight13(line_counter2),Press13(line_counter2),Temp13(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(27,*) Hight13(i),Press13(i),Temp13(i)
    ENDDO

    CLOSE(27)

    ! Output file
    OPFIL14 = 'atmosfile/LUTable_T1_285_T2_200_PRS1096.atmos'
    OPEN (28,FILE=OPFIL14,FORM='FORMATTED',STATUS='NEW')
    WRITE(28,*) line_counter2
    DO i = 1, line_counter2
        WRITE(28,*) Hight13(i), Temp13(i), Press13(i)/100.
    enddo
    CLOSE(28)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile15 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS1300.txt'
    OPEN(29, FILE = Openfile15,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(29,*,end = 112)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    112 continue
    rewind(29)
    allocate(Hight14(line_counter2),Press14(line_counter2),Temp14(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(29,*) Hight14(i),Press14(i),Temp14(i)
    ENDDO

    CLOSE(29)

    ! Output file
    OPFIL15 = 'atmosfile/LUTable_T1_285_T2_200_PRS1300.atmos'
    OPEN (30,FILE=OPFIL15,FORM='FORMATTED',STATUS='NEW')
    WRITE(30,*) line_counter2
    DO i = 1, line_counter2
        WRITE(30,*) Hight14(i), Temp14(i), Press14(i)/100.
    enddo
    CLOSE(30)

        ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile16 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/LUTable_T1_285_T2_200_PRS1500.txt'
    OPEN(31, FILE = Openfile16,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(31,*,end = 113)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    113 continue
    rewind(31)
    allocate(Hight15(line_counter2),Press15(line_counter2),Temp15(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(31,*) Hight15(i),Press15(i),Temp15(i)
    ENDDO

    CLOSE(31)

    ! Output file
    OPFIL16 = 'atmosfile/LUTable_T1_285_T2_200_PRS1500.atmos'
    OPEN (32,FILE=OPFIL16,FORM='FORMATTED',STATUS='NEW')
    WRITE(32,*) line_counter2
    DO i = 1, line_counter2
        WRITE(32,*) Hight15(i), Temp15(i), Press15(i)/100.
    enddo
    CLOSE(32)










    ! --------------------------- .k file makeing  binaryfile ---------------------------
    ! Input file
    Openfile17 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_Kw/Kw_LUTable_T1_260_T2_80_PRS50.txt'
    OPEN(33, FILE = Openfile17,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    allocate(Kw1(101100,31))  ! メモリの確保ができたら、そのファイルを読み込む
    DO j=1,101100
        READ(33,*) (Kw1(j,k),k=1,31)
!        WRITE(*,*) (Kw(k,j),k=1,101100)
    ENDDO
    CLOSE(33)

    ! Output File
    OPFIL17 = 'kfile/LUTable_T1_285_T2_200_PRS50.k'
    OPEN (34,FILE=OPFIL17,FORM='UNFORMATTED',access='stream',STATUS='replace')
    DO i = line_counter2, 1, -1
        WRITE(34) 1, real(Hight1(i)), real(Temp1(i)), real(Press1(i)/100.), real(Kw1(:,i)*1E5)
    ENDDO
    CLOSE(34)

    ! --------------------------- .k file makeing  binaryfile ---------------------------
    ! Input file
    Openfile18 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_Kw/Kw_LUTable_T1_260_T2_80_PRS150.txt'
    OPEN(35, FILE = Openfile18,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    allocate(Kw2(101100,31))  ! メモリの確保ができたら、そのファイルを読み込む
    DO j=1,101100
        READ(35,*) (Kw2(j,k),k=1,31)
!        WRITE(*,*) (Kw(k,j),k=1,101100)
    ENDDO
    CLOSE(35)

    ! Output File
    OPFIL18 = 'kfile/LUTable_T1_285_T2_200_PRS150.k'
    OPEN (36,FILE=OPFIL18,FORM='UNFORMATTED',access='stream',STATUS='replace')
    DO i = line_counter2, 1, -1
        WRITE(36) 1, real(Hight2(i)), real(Temp2(i)), real(Press2(i)/100.), real(Kw2(:,i)*1E5)
    ENDDO
    CLOSE(36)

    ! --------------------------- .k file makeing  binaryfile ---------------------------
    ! Input file
    Openfile19 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_Kw/Kw_LUTable_T1_260_T2_80_PRS180.txt'
    OPEN(37, FILE = Openfile19,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    allocate(Kw3(101100,31))  ! メモリの確保ができたら、そのファイルを読み込む
    DO j=1,101100
        READ(37,*) (Kw3(j,k),k=1,31)
!        WRITE(*,*) (Kw(k,j),k=1,101100)
    ENDDO
    CLOSE(37)

    ! Output File
    OPFIL19 = 'kfile/LUTable_T1_285_T2_200_PRS180.k'
    OPEN (38,FILE=OPFIL19,FORM='UNFORMATTED',access='stream',STATUS='replace')
    DO i = line_counter2, 1, -1
        WRITE(38) 1, real(Hight3(i)), real(Temp3(i)), real(Press3(i)/100.), real(Kw3(:,i)*1E5)
    ENDDO
    CLOSE(38)
    
end program Converter
