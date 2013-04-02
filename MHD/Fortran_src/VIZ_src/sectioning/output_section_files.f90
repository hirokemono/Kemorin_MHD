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
      use m_parallel_var_dof
      use m_multi_ucd_data
      use output_multi_ucd
      use link_psf_iso_2_write
!
      implicit  none
!
      integer(kind = kint), parameter,  private :: rank0 = 0
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_psf_grids
!
!
      if (my_rank .ne. rank0) return
      call link_psf_outputs
      call output_multi_ucd_grids
      call unlink_multl_ucd_data
!
      end subroutine output_psf_grids
!
! ----------------------------------------------------------------------
!
      subroutine output_psf_fields(istep_psf)
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      if (my_rank .ne. rank0) return
      call link_psf_outputs
      call output_multi_ucd_fields(istep_psf)
      call unlink_multl_ucd_data
!
      end subroutine output_psf_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_iso_ucds(istep_iso)
!
      integer(kind = kint), intent(in) :: istep_iso
!
!
      if (my_rank .ne. rank0) return
      call link_iso_outputs
      call output_multi_ucd_fields(istep_iso)
      call unlink_multl_ucd_data
!
      end subroutine output_iso_ucds
!
! ----------------------------------------------------------------------
!
      end module output_section_files
