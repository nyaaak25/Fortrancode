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
    character (LEN=1000) :: open_file,OPFIL0 ! 基本最初に型指定。長さの限界が100文字
    INTEGER :: i,N1=10
    DOUBLE PRECISION :: d,e,WN,NCH
    double precision,allocatable::A(:),B(:)

    ! Input file
    open_file = '/Users/nyonn/Desktop/pythoncode/4545-5556_0.01step_cutoff_120.txt'
    OPEN(2, FILE = open_file,STATUS='old')
    READ(2,*) N1
    ALLOCATE(A(1:N1),B(1:N1))
    DO i=1,N1
        read(2,*) A(i),B(i)
    enddo 

    READ(2,*) A,B ! READは格納しないといけなくて、配列の形のまま入れないといけない
    CLOSE(2)

    print *, A

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