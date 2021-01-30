!>@file   t_structure_4_interolation.f90
!!@brief  module t_structure_4_interolation
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!!
!>@brief Structure for interpolation betwen meshes
!!
!!@verbatim
!!      subroutine s_input_control_interpolate(itp_d, ierr)
!!        type(structure_4_interolation), intent(inout) :: itp_d
!!      subroutine set_ctl_interpolate_udt(fld_ctl, nod_fld)
!!        type(field_control), intent(in) :: fld_ctl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!!
      module t_structure_4_interolation
!
      use m_precision
!
      use t_ctl_data_gen_table
      use t_ctl_params_4_gen_table
      use t_step_parameter
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_interpolate_table
      use t_vector_for_solver
!
      implicit none
!
      type structure_4_interolation
        integer :: nprocs_2nd
!
        type(ctl_data_gen_table) :: gtbl_ctl
        type(ctl_params_4_gen_table) :: gen_itp_p
!
        type(time_step_param) :: t_ITP
!
        type(mesh_data) :: org_fem
        type(mesh_data) :: new_fem
!
        type(interpolate_table) :: itp_tbl
!
        type(phys_address) :: iphys
        type(SGS_model_addresses) :: iphys_LES
!
        type(phys_data) :: org_fld
        type(phys_data) :: new_fld
!
        type(vectors_4_solver) :: v_1st_sol
        type(vectors_4_solver) :: v_2nd_sol
      end type structure_4_interolation
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_input_control_interpolate(itp_d, ierr)
!
      use t_IO_step_parameter
!
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
      type(structure_4_interolation), intent(inout) :: itp_d
      integer(kind = kint), intent(inout) :: ierr
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_interpolate'
      call read_control_4_interpolate(itp_d%gtbl_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_interpolation'
      call set_ctl_params_interpolation                                 &
     &   (itp_d%gtbl_ctl, itp_d%gen_itp_p, itp_d%nprocs_2nd)
!
      call set_fixed_time_step_params                                   &
     &   (itp_d%gtbl_ctl%t_gt_ctl, itp_d%t_ITP, ierr, e_message)
!
!  --  read geometry for origin (if exist)
!
      if (my_rank .lt. itp_d%gen_itp_p%ndomain_org) then
        call input_mesh(itp_d%gen_itp_p%itp_org_mesh_file, my_rank,     &
     &     itp_d%org_fem%mesh, itp_d%org_fem%group, ierr)
!
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos                                      &
     &     (itp_d%org_fem%mesh%node, itp_d%org_fem%mesh%ele)
      end if
!
!  --  read 2nd mesh for target (if exist)
!
!
      if (my_rank .lt. itp_d%gen_itp_p%ndomain_dest) then
        call input_mesh(itp_d%gen_itp_p%itp_dest_mesh_file, my_rank,    &
     &      itp_d%new_fem%mesh, itp_d%new_fem%group, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Target mesh data is wrong!!')
        end if
!
        call alloc_overlapped_ele(itp_d%new_fem%mesh%ele)
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
     &   (my_rank, itp_d%gen_itp_p%itp_file_IO, itp_d%itp_tbl)
!
      if (iflag_debug.eq.1) write(*,*) 'init_interpolate_nodal_data'
      call init_interpolate_nodal_data                                  &
     &   (itp_d%org_fem%mesh%node, itp_d%org_fem%mesh%ele,              &
     &    itp_d%new_fem%mesh%node, itp_d%itp_tbl,                       &
     &    itp_d%v_1st_sol, itp_d%v_2nd_sol)
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
      end module t_structure_4_interolation
