!refine_interpolate_para_tbl.f90
!
      program refine_interpolate_para_tbl
!
      use m_precision
!
      use analyzer_refine_itp_para
!
      implicit none
!
!
      call init_refine_itp_para
!
      call  analyze_refine_itp_para
!
      stop
      end program refine_interpolate_para_tbl
