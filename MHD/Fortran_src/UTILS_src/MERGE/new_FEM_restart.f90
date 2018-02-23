!>@file  new_FEM_restart.f90
!!       module new_FEM_restart
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in ????
!
!> @brief Routines to construct new restart data for FEM
!!
!!@verbatim
!!      subroutine count_restart_data_fields                            &
!!     &         (org_fst_param, mgd_mesh, t_IO, merged_IO)
!!      subroutine generate_new_restart_snap                            &
!!     &         (istep, org_fst_param, new_fst_param,                  &
!!     &          mgd_mesh, t_IO, merged_IO)
!!        type(field_IO_params), intent(in) :: org_fst_param
!!        type(field_IO_params), intent(in) :: new_fst_param
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: merged_IO
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!!
!!      subroutine init_by_old_restart_data                             &
!!     &         (org_fst_param, mgd_mesh, t_IO, merged_IO)
!!      subroutine update_restart_file                                  &
!!     &         (istep, org_fst_param, new_fst_param,                  &
!!     &          mgd_mesh, t_IO, merged_IO)
!!        type(field_IO_params), intent(in) :: org_fst_param
!!        type(field_IO_params), intent(in) :: new_fst_param
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: merged_IO
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!!
!!      subroutine delete_old_restart(istep, org_fst_param)
!!@endverbatim
!
      module new_FEM_restart
!
      use m_precision
      use m_constants
      use m_control_param_merge
      use m_file_format_switch
      use t_mesh_data_4_merge
      use t_file_IO_parameter
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_restart_data_fields                              &
     &         (org_fst_param, mgd_mesh, t_IO, merged_IO)
!
      use field_IO_select
      use set_field_to_restart
!
      type(field_IO_params), intent(in) :: org_fst_param
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: merged_IO
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      call sel_read_alloc_FEM_fld_head                                  &
     &   (mgd_mesh%num_pe, izero, istep_start, org_fst_param,           &
     &    t_IO, merged_IO)
!
      call init_field_name_by_restart(merged_IO, mgd_mesh%merged_fld)
      call alloc_phys_data_type                                         &
     &   (mgd_mesh%merged%node%numnod, mgd_mesh%merged_fld)
!
      end subroutine count_restart_data_fields
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine generate_new_restart_snap                              &
     &         (istep, org_fst_param, new_fst_param,                    &
     &          mgd_mesh, t_IO, merged_IO)
!
      use m_2nd_geometry_4_merge
!
      use set_merged_restart_data
      use field_IO_select
!
      integer (kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: org_fst_param
      type(field_IO_params), intent(in) :: new_fst_param
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: merged_IO
      type(merged_mesh), intent(inout) :: mgd_mesh
!
      integer (kind = kint) :: ip, id_rank
!
!
      do ip = 1, mgd_mesh%num_pe
        id_rank = ip - 1
!
        merged_IO%nnod_IO = mgd_mesh%subdomain(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
        call sel_read_step_FEM_field_file                               &
     &     (mgd_mesh%num_pe, id_rank, istep, org_fst_param,             &
     &      t_IO, merged_IO)
        call set_restart_data_2_merge                                   &
     &     (mgd_mesh%subdomain(ip), merged_IO, mgd_mesh%merged_fld)
!
        call dealloc_phys_data_IO(merged_IO)
      end do
!
!   re-scaling for magnetic field
!
      call rescale_4_magne(mgd_mesh%merge_tbl, mgd_mesh%merged_fld)
!
!   output new restart data
!
      call alloc_merged_field_stack(sec_mesh1%num_pe2, merged_IO)
!
      merged_IO%istack_numnod_IO(0) = 0
      do ip = 1, sec_mesh1%num_pe2
        merged_IO%istack_numnod_IO(ip)                                  &
     &      = merged_IO%istack_numnod_IO(ip-1)                          &
     &       + sec_mesh1%subdomains_2(ip)%node%numnod
      end do
!
      do ip = 1, sec_mesh1%num_pe2
        id_rank = ip - 1
!
        merged_IO%nnod_IO = sec_mesh1%subdomains_2(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
        call set_new_restart_data(mgd_mesh%merged_fld,                  &
     &      sec_mesh1%subdomains_2(ip), merged_IO)
!
        call sel_write_step_FEM_field_file                              &
     &     (sec_mesh1%num_pe2, id_rank, istep, new_fst_param,           &
     &      t_IO, merged_IO)
        call dealloc_phys_data_IO(merged_IO)
      end do
      call dealloc_merged_field_stack(merged_IO)
!
      end subroutine generate_new_restart_snap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_by_old_restart_data                               &
     &         (org_fst_param, mgd_mesh, t_IO, merged_IO)
!
      use input_old_file_sel_4_zlib
      use set_field_to_restart
!
      type(field_IO_params), intent(in) :: org_fst_param
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: merged_IO
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      call sel_read_rst_comps                                           &
     &   (izero, istep_start, org_fst_param, t_IO, merged_IO)
!
      call init_field_name_by_restart(merged_IO, mgd_mesh%merged_fld)
      call alloc_phys_data_type                                         &
     &   (mgd_mesh%merged%node%numnod, mgd_mesh%merged_fld)
!
      end subroutine init_by_old_restart_data
!
!  ---------------------------------------------------------------------
!
      subroutine update_restart_file                                    &
     &         (istep, org_fst_param, new_fst_param,                    &
     &          mgd_mesh, t_IO, merged_IO)
!
      use m_2nd_geometry_4_merge
!
      use set_merged_restart_data
      use field_IO_select
      use input_old_file_sel_4_zlib
!
      integer (kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: org_fst_param
      type(field_IO_params), intent(in) :: new_fst_param
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: merged_IO
      type(merged_mesh), intent(inout) :: mgd_mesh
!
      integer (kind = kint) :: ip, id_rank
!
!
      do ip = 1, mgd_mesh%num_pe
        id_rank = ip - 1
!
        merged_IO%nnod_IO = mgd_mesh%subdomain(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
!
        call sel_read_rst_file                                          &
     &     (id_rank, istep, org_fst_param, t_IO, merged_IO)
!
        call set_restart_data_2_merge                                   &
     &     (mgd_mesh%subdomain(ip), merged_IO, mgd_mesh%merged_fld)
!
        call dealloc_phys_data_IO(merged_IO)
      end do
!
!   re-scaling for magnetic field
!
      call rescale_4_magne(mgd_mesh%merge_tbl, mgd_mesh%merged_fld)
!
!   output new restart data
!
      call alloc_merged_field_stack(sec_mesh1%num_pe2, merged_IO)
!
      merged_IO%istack_numnod_IO(0) = 0
      do ip = 1, sec_mesh1%num_pe2
        merged_IO%istack_numnod_IO(ip)                                  &
     &      = merged_IO%istack_numnod_IO(ip-1)                          &
     &       + sec_mesh1%subdomains_2(ip)%node%numnod
      end do
!
      do ip = 1, sec_mesh1%num_pe2
        id_rank = ip - 1
!
        merged_IO%nnod_IO = sec_mesh1%subdomains_2(ip)%node%numnod
        call alloc_phys_data_IO(merged_IO)
!
        call set_new_restart_data(mgd_mesh%merged_fld,                  &
     &      sec_mesh1%subdomains_2(ip), merged_IO)
!
        call sel_write_step_FEM_field_file                              &
     &     (sec_mesh1%num_pe2, id_rank, istep, new_fst_param,           &
     &      t_IO, merged_IO)
        call dealloc_phys_data_IO(merged_IO)
      end do
      call dealloc_merged_field_stack(merged_IO)
!
      end subroutine update_restart_file
!
!  ---------------------------------------------------------------------
!
      subroutine delete_old_restart(istep, num_pe, org_fst_param)
!
      use delete_data_files
      use set_parallel_file_name
!
      type(field_IO_params), intent(in) :: org_fst_param
      integer (kind = kint), intent(in) :: istep, num_pe
      character(len=kchara) :: fname_c
!
!
      call add_int_suffix(istep, org_fst_param%file_prefix, fname_c)
      call delete_parallel_files                                        &
     &   (org_fst_param%iflag_format, num_pe, fname_c)
!
      end subroutine delete_old_restart
!
! -----------------------------------------------------------------------
!
      end module new_FEM_restart
