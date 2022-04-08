! pythonで作成した吸収係数を入れて、binary fileに変換する。

program Converter
    implicit none  ! おまじないで絶対に必要なもの

    ! parameterは最初に定義しておくと、あとから更新が可能。コンスタントは更新不可。
    ! do loopを回して、読み込みができるよ。
    ! integerがint型。[2022.4.8 18:40 Kazama]

    character (LEN=100) :: OPFIL0 ! 基本最初に型指定。長さの限界が100文字
    character (LEN=100) :: open_file
    INTEGER :: a,b,c,d,e,aa,bb,cc,dd,ee

    ! Input file
    open_file = '/Users/nyonn/Desktop/pythoncode/4545-5556_0.01step_cutoff_120.txt'
    OPEN(2, FILE = open_file,STATUS='old')
    READ(2,*) a ! READは格納しないといけなくて、配列の形のまま入れないといけない
    READ(2,*) b
    READ(2,*) c
    READ(2,*) d
    READ(2,*) e
    CLOSE(2)

    ! Output File
    OPFIL0 = 'vfile/LUtable.v'
    OPEN (3,FILE=OPFIL0,FORM='UNFORMATTED',STATUS='replace')
    WRITE(3) 1, 1
    WRITE(3) 1, NCH
    WRITE(3) WN
    CLOSE(3)

    
end program Converter