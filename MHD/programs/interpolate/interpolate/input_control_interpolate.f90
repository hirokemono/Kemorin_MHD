!
!      module input_control_interpolate
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine s_input_control_interpolate(gen_itp_p, gtbl_ctl,     &
!!     &          org_femmesh, new_femmesh, itp_info, t_param, ierr)
!!        type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
!!        type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
!!        type(mesh_data), intent(inout) :: org_femmesh
!!        type(mesh_data), intent(inout) :: new_femmesh
!!        type(interpolate_table), intent(inout) :: itp_info
!!        type(time_step_param), intent(inout) :: t_param
!      subroutine set_ctl_interpolate_udt(fld_ctl, nod_fld)
!        type(field_control), intent(in) :: fld_ctl
!        type(phys_data), intent(inout) :: nod_fld
!
      module input_control_interpolate
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_interpolate(gen_itp_p, gtbl_ctl,       &
     &          org_femmesh, new_femmesh, itp_info, t_param, ierr)
!
      use t_mesh_data
      use t_interpolate_table
      use t_step_parameter
      use t_IO_step_parameter
      use t_ctl_data_gen_table
      use t_ctl_params_4_gen_table
!
      use m_2nd_pallalel_vector
      use m_interpolate_table_IO
!
      use set_ctl_interpolation
!
      use load_mesh_data
      use const_mesh_information
!
      use itp_table_IO_select_4_zlib
      use copy_interpolate_types
      use interpolate_nod_field_2_type
      use read_ctl_data_gen_table
!
      type(ctl_params_4_gen_table), intent(inout) :: gen_itp_p
      type(ctl_data_gen_table), intent(inout) :: gtbl_ctl
      type(mesh_data), intent(inout) :: org_femmesh
      type(mesh_data), intent(inout) :: new_femmesh
      type(interpolate_table), intent(inout) :: itp_info
      type(time_step_param), intent(inout) :: t_param
      integer(kind = kint), intent(inout) :: ierr
!
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_interpolate'
      call read_control_4_interpolate(gtbl_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_interpolation'
      call set_ctl_params_interpolation(gtbl_ctl, gen_itp_p)
!
      call set_fixed_time_step_params                                   &
     &   (gtbl_ctl%t_gt_ctl, t_param, ierr, e_message)
!
!  --  read geometry for origin (if exist)
!
      if (my_rank .lt. gen_itp_p%ndomain_org) then
        call input_mesh(gen_itp_p%itp_org_mesh_file, my_rank,           &
     &     org_femmesh%mesh, org_femmesh%group, ierr)
!
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos                                      &
     &     (org_femmesh%mesh%node, org_femmesh%mesh%ele)
      end if
!
!  --  read 2nd mesh for target (if exist)
!
!
      if (my_rank .lt. gen_itp_p%ndomain_dest) then
        call input_mesh(gen_itp_p%itp_dest_mesh_file, my_rank,          &
     &      new_femmesh%mesh, new_femmesh%group, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Target mesh data is wrong!!')
        end if
!
        call alloc_overlapped_ele(new_femmesh%mesh%ele)
      end if
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
!  --  read interpolate table
!
      if (iflag_debug.eq.1) write(*,*) 'load_interpolate_table'
      call load_interpolate_table                                       &
     &   (my_rank, gen_itp_p%itp_file_IO, itp_info)
!
      if (iflag_debug.eq.1) write(*,*) 'init_interpolate_nodal_data'
      call init_interpolate_nodal_data                                  &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    new_femmesh%mesh%node, itp_info)
!
      end subroutine s_input_control_interpolate
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_interpolate_udt(fld_ctl, nod_fld)
!
      use calypso_mpi
      use t_phys_data
      use t_ctl_data_4_fields
      use set_field_data_w_SGS
!
      type(field_control), intent(in) :: fld_ctl
      type(phys_data), intent(inout) :: nod_fld
      integer(kind = kint) :: ierr
!
!
      call set_SGS_field_ctl_by_viz(fld_ctl%field_ctl, nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine set_ctl_interpolate_udt
!
! ----------------------------------------------------------------------
!
      end module input_control_interpolate
