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
      use m_parallel_var_dof
      use m_control_parameter
      use m_t_step_parameter
      use m_ucd_data
!
      use set_ucd_data
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: i_step
!
!
      ucd_step = i_step / i_step_output_ucd
!
      if (i_step .eq. (ucd_step*i_step_output_ucd) ) then
!
        fem_ucd%file_prefix = org_ucd_header
        call link_num_field_2_output
        call sel_read_udt_param(my_rank, ucd_step, fem_ucd)
        call set_ucd_data_from_IO(my_rank, ucd_step)
!
        call deallocate_ucd_data(fem_ucd)
      end if
!
      end subroutine read_udt_4_snap
!
! ----------------------------------------------------------------------
!
      end module read_udt_4_snapshot
