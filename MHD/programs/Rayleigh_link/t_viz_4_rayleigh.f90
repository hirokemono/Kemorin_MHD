!>@file   t_viz_4_rayleigh.f90
!!@brief  module t_viz_4_rayleigh
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_ctl_params_rayleigh_viz(rayleigh_vctl,           &
!!     &          t_viz_param, FEM_Rayleigh, ierr)
!!        type(control_data_rayleigh_vizs), intent(in) :: rayleigh_vctl
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!        type(FEM_mesh_field_rayleigh_viz),                            &
!!     &                  intent(inout) :: FEM_Rayleigh
!!@endverbatim
!
      module t_viz_4_rayleigh
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_vector_for_solver
      use t_VIZ_only_step_parameter
!
      use t_rayleigh_field_IO
      use t_rayleigh_field_address
!
      implicit none
!
!>      Structure of FEM mesh and field structures
      type FEM_mesh_field_rayleigh_viz
!>        Structure for FEM mesh data
!!         (position, connectivity, communication, and groups)
        type(mesh_data) :: geofem
!>       Structure for nodal field data
        type(phys_data) :: field
!
!>        Structure for vectors for solver
        type(vectors_4_solver) :: v_sol
!
        type(rayleigh_field) :: rayleigh_rtp
        type(rayleigh_field_address) :: iphys_ftb
      end type FEM_mesh_field_rayleigh_viz
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_rayleigh_viz(rayleigh_vctl,             &
     &          t_viz_param, FEM_Rayleigh, ierr)
!
      use m_error_IDs
      use t_file_IO_parameter
      use t_ctl_data_rayleigh_vizs
!
      use m_file_format_switch
      use set_control_platform_item
!
      type(control_data_rayleigh_vizs), intent(in) :: rayleigh_vctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      type(FEM_mesh_field_rayleigh_viz), intent(inout) :: FEM_Rayleigh
!
!
      call set_control_smp_def(my_rank, rayleigh_vctl%viz_plt)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (rayleigh_vctl%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
      if(ierr .gt. 0) return
!
      call set_ctl_rayleigh_field_address                               &
     &   (rayleigh_vctl%viz_plt, rayleigh_vctl%fld_ctl,                 &
     &    FEM_Rayleigh%iphys_ftb, e_message, ierr)
!
      call set_ctl_params_rayleigh_domains                              &
     &   (rayleigh_vctl%sdctl, FEM_Rayleigh%rayleigh_rtp,               &
     &    e_message, ierr)
!
      end subroutine set_ctl_params_rayleigh_viz
!
! ----------------------------------------------------------------------
!
      end module t_viz_4_rayleigh
