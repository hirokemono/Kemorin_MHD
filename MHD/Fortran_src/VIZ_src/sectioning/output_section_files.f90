!output_section_files.f90
!      module output_section_files
!
!      Written by H. Matsui on July, 2006
!
!      subroutine output_psf_grids
!      subroutine output_psf_fields(istep_psf)
!
!      subroutine output_iso_ucds(istep_iso)
!
      module output_section_files
!
      use m_precision
!
      use calypso_mpi
      use ucd_IO_select
!
      implicit  none
!
      integer(kind = kint), parameter, private :: rank0 = 0
      integer(kind = kint), parameter, private :: delete_process = -1
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_psf_grids
!
      use m_control_params_4_psf
      use m_psf_outputs
!
      integer(kind = kint) :: i_psf
!
      if (my_rank .ne. rank0) return
!
      do i_psf = 1, num_psf
        call sel_write_grd_file(delete_process, psf_out(i_psf))
      end do
!
      end subroutine output_psf_grids
!
! ----------------------------------------------------------------------
!
      subroutine output_psf_fields(istep_psf)
!
      use m_control_params_4_psf
      use m_psf_outputs
!
      integer(kind = kint), intent(in) :: istep_psf
      integer(kind = kint) :: i_psf
!
!
      if (my_rank .ne. rank0) return
      do i_psf = 1, num_psf
        call sel_write_udt_file(delete_process, istep_psf,              &
     &      psf_out(i_psf))
      end do
!
      end subroutine output_psf_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_iso_ucds(istep_iso)
!
      use m_control_params_4_iso
      use m_iso_outputs
!
      integer(kind = kint), intent(in) :: istep_iso
      integer(kind = kint) :: i_iso
!
!
      if (my_rank .ne. rank0) return
      do i_iso = 1, num_iso
        call sel_write_ucd_file(delete_process, istep_iso,              &
    &       iso_out(i_iso))
      end do
!
      end subroutine output_iso_ucds
!
! ----------------------------------------------------------------------
!
      end module output_section_files
