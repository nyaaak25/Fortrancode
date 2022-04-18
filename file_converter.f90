! pythonで作成した吸収係数を入れて、binary fileに変換するプログラム
! @author: A.Kazama kazama@pparc.gp.tohoku.ac.jp


! ver. 1.0: binary：　pythonのfileを読み込んで、 .v fileを作成する
! Created on Sun Apr 10 16:10:00 2022

! ver. 1.1: binary:  pythonのfileを読み込んで、 .atmos file, .k fileを作成する
! Created on Wed Apr 13 10:19:00 2022


program Converter
    implicit none  ! おまじないで絶対に必要なもの

    ! parameter指定
    character (LEN=1000) :: Openfile01,Openfile02,Openfile03,OPFIL0,OPFIL1,OPFIL2 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i,j,k,line_counter1, line_counter2 ! ALLOCATABLEはあとで確保してあげようとしている。確保しないと使えない。
    double precision,allocatable::WN(:),Kw(:,:),Hight(:),Temp(:),Press(:)


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
    OPFIL0 = 'vfile/LUtable.v'
    OPEN (9,FILE=OPFIL0,FORM='UNFORMATTED',access='stream',STATUS='replace')
    WRITE(9) 1, 1
    WRITE(9) 1, line_counter1
    WRITE(9) WN
    CLOSE(9)


    ! --------------------------- .atmos file making  txtfile ---------------------------
    ! Input file
    Openfile02 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_HTP/pre_temp_profile.txt'
    OPEN(2, FILE = Openfile02,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(2,*,end = 99)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    99 continue
    rewind(2)
    allocate(Hight(line_counter2),Press(line_counter2),Temp(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(2,*) Hight(i),Press(i),Temp(i)
    ENDDO

    CLOSE(2)

    ! Output file
    OPFIL1 = 'atmosfile/LUtable.atmos'
    OPEN (4,FILE=OPFIL1,FORM='FORMATTED',STATUS='replace')
    WRITE(4,*) line_counter2
    DO i = 1, line_counter2
        WRITE(4,*) Hight(i), Temp(i), Press(i)/100.
    enddo
    CLOSE(4)


    ! --------------------------- .k file makeing  binaryfile ---------------------------
    ! Input file
    Openfile03 = '/Users/nyonn/Desktop/pythoncode/LookUpTable_Kw/LUtable_1_Kw.txt'
    OPEN(5, FILE = Openfile03,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    allocate(Kw(101100,31))  ! メモリの確保ができたら、そのファイルを読み込む
    DO j=1,101100
        READ(5,*) (Kw(j,k),k=1,31)
!        WRITE(*,*) (Kw(k,j),k=1,101100)
    ENDDO
    CLOSE(5)

    ! Output File
    OPFIL2 = 'kfile/LUtable.k'
    OPEN (7,FILE=OPFIL2,FORM='UNFORMATTED',access='stream',STATUS='replace')
    DO i = 1, line_counter2
        WRITE(7) 1, real(Hight(i)), real(Temp(i)), real(Press(i)/100.), real(Kw(:,i)*1E5)
    ENDDO
    CLOSE(7)
    
end program Converter
