!>@file  field_data_IO_b.f90
!!       module field_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_data_b(id_rank)
!!      subroutine write_field_data_b(nnod, num_field, ntot_comp,       &
!!     &          ncomp_field, field_name, d_nod)
!!
!!      subroutine read_step_data_b(istack_merged, num_field)
!!      subroutine read_field_data_b                                    &
!!     &         (nnod, num_field, ntot_comp, field_name, vect)
!!@endverbatim
!
      module field_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_time_data_IO
      use t_field_data_IO
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_step_data_b(id_rank)
!
!
      integer(kind=kint), intent(in) :: id_rank
!
!
      call write_fld_inthead_b(id_rank)
      call write_fld_inthead_b(i_time_step_IO)
!
      call write_fld_realhead_b(time_IO)
      call write_fld_realhead_b(delta_t_IO)
!
      end subroutine write_step_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_b(nnod, num_field, ntot_comp,         &
     &          ncomp_field, field_name, d_nod)
!
      use m_phys_constants
!
      integer(kind=kint), intent(in) :: nnod, num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint_gl) :: istack_merged(1)
!
!
      istack_merged(1) = nnod
      call write_fld_mul_i8head_b(ione, istack_merged)
      call write_fld_inthead_b(num_field)
      call write_fld_mul_inthead_b(num_field, ncomp_field)
!
      call write_fld_mul_charhead_b(num_field, field_name)
      call write_fld_realarray2_b(nnod, ntot_comp, d_nod)
!
      end subroutine write_field_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_b(istack_merged, num_field)
!
      integer(kind=kint_gl), intent(inout) :: istack_merged(1)
      integer(kind=kint), intent(inout) :: num_field
!
      integer(kind = kint) :: id_rank
!
!
      call read_fld_inthead_b(id_rank)
      call read_fld_inthead_b(i_time_step_IO)
      call read_fld_realhead_b(time_IO)
      call read_fld_realhead_b(delta_t_IO)
!
      call read_fld_mul_i8head_b(ione, istack_merged)
      call read_fld_inthead_b(num_field)
!
      end subroutine read_step_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_b                                      &
     &         (nnod, num_field, ntot_comp, field_name, vect)
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ntot_comp)
!
!
      call read_fld_mul_charhead_b(num_field, field_name)
      call read_fld_realarray2_b(nnod, ntot_comp, vect)
!
      end subroutine read_field_data_b
!
! -----------------------------------------------------------------------
!
      end module field_data_IO_b
