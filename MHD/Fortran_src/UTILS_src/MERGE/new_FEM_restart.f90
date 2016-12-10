!>@file  new_FEM_restart.f90
!!       module new_FEM_restart
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in ????
!
!> @brief Routines to construct new restart data for FEM
!!
!!@verbatim
!!      subroutine count_restart_data_fields
!!      subroutine generate_new_restart_snap(istep)
!!
!!      subroutine init_by_old_restart_data
!!      subroutine update_restart_file(istep)
!!
!!      subroutine delete_restart_files(istep)
!!      subroutine delete_old_restart(istep)
!!@endverbatim
!
      module new_FEM_restart
!
      use m_precision
      use m_constants
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_file_format_switch
      use m_time_data_IO
      use t_field_data_IO
!
      implicit none
!
      type(field_IO), save, private :: merged_IO
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_restart_data_fields
!
      use field_IO_select
      use set_field_to_restart
!
      call set_field_file_fmt_prefix                                    &
     &   (iorg_rst_file_fmt, org_rst_head, merged_IO)
      call sel_read_alloc_FEM_fld_head                                  &
     &   (num_pe, izero, istep_start, t1_IO, merged_IO)
!
      call init_field_name_by_restart(merged_IO, merged_fld)
      call alloc_phys_data_type(merged%node%numnod, merged_fld)
!
      end subroutine count_restart_data_fields
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine generate_new_restart_snap(istep)
!
      use m_2nd_geometry_4_merge
!
      use set_merged_restart_data
      use field_IO_select
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, id_rank
!
!
      call set_field_file_fmt_prefix                                    &
     &   (iorg_rst_file_fmt, org_rst_head, merged_IO)
!
      do ip = 1, num_pe
        id_rank = ip - 1
!
        merged_IO%nnod_IO = subdomain(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
        call sel_read_step_FEM_field_file                               &
     &     (num_pe, id_rank, istep, t1_IO, merged_IO)
        call set_restart_data_2_merge(ip, merged_IO)
!
        call dealloc_phys_data_IO(merged_IO)
      end do
!
!   re-scaling for magnetic field
!
      call rescale_4_magne
!
!   output new restart data
!
      call alloc_merged_field_stack(num_pe2, merged_IO)
!
      merged_IO%istack_numnod_IO(0) = 0
      do ip = 1, num_pe2
        merged_IO%istack_numnod_IO(ip)                                  &
     &      = merged_IO%istack_numnod_IO(ip-1)                          &
     &       + subdomains_2(ip)%node%numnod
      end do
!
      call set_field_file_fmt_prefix                                    &
     &   (inew_rst_file_fmt, new_rst_head, merged_IO)
      do ip = 1, num_pe2
        id_rank = ip - 1
!
        merged_IO%nnod_IO =   subdomains_2(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
        call set_new_restart_data(ip, merged_IO)
!
        call sel_write_step_FEM_field_file                              &
     &     (num_pe2, id_rank, istep, t1_IO, merged_IO)
        call dealloc_phys_data_IO(merged_IO)
      end do
      call dealloc_merged_field_stack(merged_IO)
!
      end subroutine generate_new_restart_snap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_newrst_phys_name_IO
!
      call dealloc_phys_name_IO(merged_IO)
!
      end subroutine dealloc_newrst_phys_name_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_by_old_restart_data
!
      use input_old_file_sel_4_zlib
      use set_field_to_restart
!
      call set_field_file_fmt_prefix                                    &
     &   (iorg_rst_file_fmt, org_rst_head, merged_IO)
      call sel_read_rst_comps(izero, istep_start, merged_IO)
!
      call init_field_name_by_restart(merged_IO, merged_fld)
      call alloc_phys_data_type(merged%node%numnod, merged_fld)
!
      end subroutine init_by_old_restart_data
!
!  ---------------------------------------------------------------------
!
      subroutine update_restart_file(istep)
!
      use m_2nd_geometry_4_merge
!
      use set_merged_restart_data
      use field_IO_select
      use input_old_file_sel_4_zlib
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, id_rank
!
!
      call set_field_file_fmt_prefix                                    &
     &   (iorg_rst_file_fmt, org_rst_head, merged_IO)
!
      do ip = 1, num_pe
        id_rank = ip - 1
!
        merged_IO%nnod_IO = subdomain(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
!
        call sel_read_rst_file(id_rank, istep, merged_IO)
!
        call set_restart_data_2_merge(ip, merged_IO)
!
        call dealloc_phys_data_IO(merged_IO)
      end do
!
!   re-scaling for magnetic field
!
      call rescale_4_magne
!
!   output new restart data
!
      call alloc_merged_field_stack(num_pe2, merged_IO)
!
      merged_IO%istack_numnod_IO(0) = 0
      do ip = 1, num_pe2
        merged_IO%istack_numnod_IO(ip)                                  &
     &      = merged_IO%istack_numnod_IO(ip-1)                          &
     &       + subdomains_2(ip)%node%numnod
      end do
!
      call set_field_file_fmt_prefix                                    &
     &   (inew_rst_file_fmt, new_rst_head, merged_IO)
!
      do ip = 1, num_pe2
        id_rank = ip - 1
!
        merged_IO%nnod_IO =   subdomains_2(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
!
        call set_new_restart_data(ip, merged_IO)
!
        call sel_write_step_FEM_field_file                              &
     &     (num_pe2, id_rank, istep, t1_IO, merged_IO)
        call dealloc_phys_data_IO(merged_IO)
      end do
      call dealloc_merged_field_stack(merged_IO)
!
      end subroutine update_restart_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_restart_files(istep)
!
      use set_field_file_names
!
      integer (kind = kint), intent(in) :: istep
!
!
      call delete_FEM_fld_file                                          &
     &   (iorg_rst_file_fmt,  num_pe, istep, org_rst_head)
!
      end subroutine delete_restart_files
!
! -----------------------------------------------------------------------
!
      subroutine delete_old_restart(istep)
!
      use delete_data_files
      use set_parallel_file_name
!
      integer (kind = kint), intent(in) :: istep
      character(len=kchara) :: fname_c
!
!
      call add_int_suffix(istep, org_rst_head, fname_c)
      call delete_parallel_files(iorg_rst_file_fmt,  num_pe, fname_c)
!
      end subroutine delete_old_restart
!
! -----------------------------------------------------------------------
!
      end module new_FEM_restart
