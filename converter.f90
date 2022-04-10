! このファイルはメモ+思考整理用のものです！

! pythonで作成した吸収係数を入れて、binary fileに変換するプログラム
! @author: A.Kazama kazama@pparc.gp.tohoku.ac.jp


! ver. 1.0: binary：　pythonのfileを読み込んで、 .v fileを作成する
! Created on Sat Apr 9 08:46:00 2022


program Converter
    implicit none  ! おまじないで絶対に必要なもの

    ! parameterは最初に定義しておくと、あとから更新が可能。コンスタントは更新不可。
    ! do loopを回して、読み込みができるよ。
    ! integerがint型。[2022.4.8 18:40 Kazama]

    ! parameter指定
    ! 一番最初に本当ならば使う配列やメモリを確保する必要がある
    character (LEN=1000) :: Openfile01,OPFIL0 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i,N1=3
    INTEGER :: line_counter
    INTEGER,ALLOCATABLE::CC(:)  ! ALLOCATABLEはあとで確保してあげようとしている。確保しないと使えない。
    DOUBLE PRECISION :: d,e,WN,NCH
    double precision,allocatable::A(:),B(:)

    ! Input file
    Openfile01 = 'vfile/test.txt'
    OPEN(1, FILE = Openfile01,STATUS='old')  ! 最初にメモリ確保する必要があるから、最初に配列数を知る必要がある
    line_counter = 0
    do
        READ(1,*,end = 999)  ! ただのシグナルだけど、flagのような役割でREADでここまで来たら飛ぶような役割
        line_counter = line_counter+1  ! 列をわかるようにしている
    enddo
    999 continue
    rewind(1)
    allocate(CC(line_counter))  ! メモリの確保ができたら、そのファイルを読み込む

    DO i=1,line_counter
        READ(1,*) CC(i)
    ENDDO
    !READ(1,*) CC
    CLOSE(1)

    print *, CC

    ! open_file = '/Users/nyonn/Desktop/pythoncode/4545-5556_0.01step_cutoff_120.txt'
    ! OPEN(2, FILE = open_file,STATUS='old')
    ! READ(2,*) N1
    ! ALLOCATE(A(1:N1),B(1:N1))
    ! DO i=1,N1
    !    read(2,*) A(i),B(i)
    ! enddo 

    ! READ(2,*) A,B ! READは格納しないといけなくて、配列の形のまま入れないといけない
    ! CLOSE(2)

    ! Output File
    OPFIL0 = 'vfile/LUtable.v'
    OPEN (3,FILE=OPFIL0,FORM='UNFORMATTED',STATUS='replace')
    WRITE(3) 1, 1
    WRITE(3) 1, NCH
    WRITE(3) WN
    CLOSE(3)

    
end program Converter


! do loopを使ってtest.txtを読み込んでみる。print* ,test(1:3)とかでどんな出力結果が出るかを確認する。
! 配列の動的わりつけに気をつける。
! 今はinputのintegerで怒られている。Fortran runtime error: Bad integer for item 1 in list input