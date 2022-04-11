! pythonで作成した吸収係数を入れて、binary fileに変換するプログラム
! @author: A.Kazama kazama@pparc.gp.tohoku.ac.jp


! ver. 1.0: binary：　pythonのfileを読み込んで、 .v fileを作成する
! Created on Sun Apr 10 16:10:00 2022

! ver. 1.1: binary:  pythonのfileを読み込んで、 .k fileを作成する
! Created on xxxxx  comoing soon...


program Converter
    implicit none  ! おまじないで絶対に必要なもの

    ! parameter指定
    character (LEN=1000) :: Openfile01,Openfile02,OPFIL0,OPFIL1 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i, line_counter1, line_counter2 ! ALLOCATABLEはあとで確保してあげようとしている。確保しないと使えない。
    double precision,allocatable::WN(:),Kw(:),Hight(:),Temp(:),press(:)


    ! .v file makeing
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
    allocate(WN(line_counter1),Kw(line_counter1))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter1
        READ(1,*) WN(i),Kw(i)
    ENDDO

    CLOSE(1)

    ! print *, WN

    ! Output File
    OPFIL0 = 'vfile/LUtable.v'
    OPEN (3,FILE=OPFIL0,FORM='UNFORMATTED',STATUS='replace')
    WRITE(3) 1, 1
    WRITE(3) 1, line_counter1
    WRITE(3) WN
    CLOSE(3)

    ! Input file
    Openfile02 = '/Users/nyonn/Desktop/pythoncode/pre_temp_profile.txt'
    OPEN(2, FILE = Openfile02,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter2 = 0
    do
        READ(2,*,end = 99)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter2 = line_counter2+1  ! 列をわかるようにしている
    enddo
    99 continue
    rewind(2)
    allocate(Hight(line_counter2),Temp(line_counter2),press(line_counter2))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter2
        READ(2,*) Hight(i),Temp(i),press(i)
    ENDDO

    CLOSE(2)

    ! .k file making
    OPFIL1 = 'kfile/LUtable.k'
    OPEN (4,FILE=OPFIL1,FORM='UNFORMATTED',STATUS='replace')
    WRITE(4) line_counter2
    WRITE(4) Hight, Temp, press
    CLOSE(4)
    
end program Converter
