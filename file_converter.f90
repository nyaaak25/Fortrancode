! pythonで作成した吸収係数を入れて、binary fileに変換するプログラム
! @author: A.Kazama kazama@pparc.gp.tohoku.ac.jp


! ver. 1.0: binary：　pythonのfileを読み込んで、 .v fileを作成する
! Created on Sun Apr 10 16:10:00 2022

! ver. 1.1: binary:  pythonのfileを読み込んで、 .k fileを作成する
! Created on xxxxx  comoing soon...


program Converter
    implicit none  ! おまじないで絶対に必要なもの

    ! parameter指定
    character (LEN=1000) :: Openfile01,OPFIL0,Openfile02 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i, line_counter ! ALLOCATABLEはあとで確保してあげようとしている。確保しないと使えない。
    double precision,allocatable::WN(:),Kw(:)

    ! Input file
    Openfile01 = '/Users/nyonn/Desktop/pythoncode/4545-5556_0.01step_cutoff_120.txt'
    OPEN(1, FILE = Openfile01,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter = 0
    do
        READ(1,*,end = 999)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter = line_counter+1  ! 列をわかるようにしている
    enddo
    999 continue
    rewind(1)
    allocate(WN(line_counter),Kw(line_counter))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter
        READ(1,*) WN(i),Kw(i)
    ENDDO

    CLOSE(1)

    ! print *, WN

    ! Output File
    OPFIL0 = 'vfile/LUtable.v'
    OPEN (3,FILE=OPFIL0,FORM='UNFORMATTED',STATUS='replace')
    WRITE(3) 1, 1
    WRITE(3) 1, line_counter
    WRITE(3) WN
    CLOSE(3)
    
end program Converter
