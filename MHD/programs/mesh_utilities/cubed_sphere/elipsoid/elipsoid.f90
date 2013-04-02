!
!
      program cutshell

      use analyzer_elipsoid
!
      use m_precision
!
      implicit none
!
      integer(kind=kint )               :: errno

      call init_analyzer(errno)
!
!C===
!C-- create LOCAL DATA

      call  analyze (errno)
      stop ' * normal termination'

      end program cutshell
