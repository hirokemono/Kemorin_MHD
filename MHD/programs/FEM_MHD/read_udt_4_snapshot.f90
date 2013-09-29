!read_udt_4_snapshot.f90
!     module read_udt_4_snapshot
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!
!      subroutine read_udt_4_snap(i_step)
!
      module read_udt_4_snapshot
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_udt_4_snap(i_step)
!
      use calypso_mpi
      use m_control_parameter
      use m_t_step_parameter
      use m_ucd_input_data
      use m_control_params_2nd_files
!
      integer(kind = kint), intent(in) :: i_step
!
!
      ucd_step = i_step / i_step_output_ucd
!
      if (mod(i_step,i_step_output_ucd) .eq. 0) then
        call set_data_by_read_ucd_once(my_rank, ucd_step,               &
    &       ifmt_org_ucd, org_ucd_header)
      end if
!
      end subroutine read_udt_4_snap
!
! ----------------------------------------------------------------------
!
      end module read_udt_4_snapshot
