!
!
      program refiner
!
      use m_precision
!
      use analyzer_refine
!
      implicit none
!
      call initialize_refine
!C===
!C-- create LOCAL DATA

      call  analyze_refine

      stop
      end program refiner
