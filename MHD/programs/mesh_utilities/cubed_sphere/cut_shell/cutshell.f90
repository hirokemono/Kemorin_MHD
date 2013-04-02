!
!
      program cutshell

      use analyzer_cutshell
!
      use m_precision
!
      implicit none
!

      call initialize_cutshell
      call  analyze_cutshell
      stop ' * normal termination'

      end program cutshell
