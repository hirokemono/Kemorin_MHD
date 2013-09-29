!main_MHD_lenghscale.f90
!     program  length_scale_MHD
!
!      Main routine for evaluating length scale from diffusion
!      Programmed by H. Matsui on 2012
!
!-----------------------------------------------------------------------
!
      program length_scale_MHD
!
      use m_precision
!
      use calypso_mpi
      use analyzer_MHD_lengthscale

      implicit none
!

      call calypso_MPI_init
!
      call initialize_MHD_lscale

      call analyze_MHD_lscale

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program length_scale_MHD
