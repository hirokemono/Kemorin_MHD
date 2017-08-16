!
!      module cal_filter_func_node
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine const_commute_filter_coefs                           &
!!     &         (file_name, mesh, jac_3d, FEM_elen, mom_nod)
!!      subroutine const_fluid_filter_coefs                             &
!!     &         (file_name, mesh, jac_3d, FEM_elen)
!!      subroutine set_simple_filter (file_name, mesh,                  &
!!     &          jac_3d, FEM_elen, dxidxs, mom_nod)
!!      subroutine set_simple_fluid_filter(file_name, mesh,             &
!!     &          jac_3d, FEM_elen, dxidxs, mom_nod)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      module cal_filter_func_node
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_jacobians
      use t_filter_elength
      use t_next_node_ele_4_node
!
      implicit none
!
      integer(kind = kint), parameter :: num_fixed_point = 0
      type(element_around_node), save :: ele_4_nod_s
      type(next_nod_id_4_nod), save :: neib_nod_s
!
      private :: ele_4_nod_s, neib_nod_s
!
      private :: num_fixed_point
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_commute_filter_coefs                             &
     &         (file_name, mesh, jac_3d, FEM_elen, mom_nod)
!
      use t_filter_moments
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
      use set_simple_filters
!
      character(len = kchara), intent(in) :: file_name
      type(mesh_geometry),       intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
      integer(kind = kint) :: inod, ierr
!
!
      call init_4_cal_fileters(mesh, ele_4_nod_s, neib_nod_s)
!
      write(70+my_rank,*) ' Best condition for filter'
!
      do inod = inod_start_filter, inod_end_filter
        call const_filter_func_nod_by_nod                               &
     &     (file_name, inod, mesh%node, mesh%ele,                       &
     &      ele_4_nod_s, neib_nod_s, jac_3d, FEM_elen, ierr)
!
        nnod_near_nod_weight(inod) = nnod_near_1nod_weight
        call cal_filter_moms_each_nod_type(inod, mom_nod)
      end do
!
      call dealloc_iele_belonged(ele_4_nod_s)
      call dealloc_inod_next_node(neib_nod_s)
!
      end subroutine const_commute_filter_coefs
!
! -----------------------------------------------------------------------
!
      subroutine const_fluid_filter_coefs                               &
     &         (file_name, mesh, jac_3d, FEM_elen)
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
!
      character(len = kchara), intent(in) :: file_name
      type(mesh_geometry),       intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer(kind = kint) :: inod, ierr
!
!
      call init_4_cal_fluid_fileters(mesh, ele_4_nod_s, neib_nod_s)
!
      write(70+my_rank,*) ' Best condition for fluid filter'
!
      do inod = inod_start_filter, inod_end_filter
        call const_fluid_filter_nod_by_nod                              &
     &     (file_name, inod, mesh%node, mesh%ele,                       &
     &      ele_4_nod_s, neib_nod_s, jac_3d, FEM_elen, ierr)
      end do
!
      call dealloc_iele_belonged(ele_4_nod_s)
      call dealloc_inod_next_node(neib_nod_s)
!
      end subroutine const_fluid_filter_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_simple_filter (file_name, mesh,                    &
     &          jac_3d, FEM_elen, dxidxs, mom_nod)
!
      use t_filter_dxdxi
      use t_filter_moments
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
      use set_simple_filters
!
      character(len = kchara), intent(in) :: file_name
      type(mesh_geometry),       intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fileters(mesh, ele_4_nod_s, neib_nod_s)
!
      i_exp_level_1nod_weight = maximum_neighbour
      do inod = inod_start_filter, inod_end_filter
        call set_simple_filter_nod_by_nod                               &
     &     (file_name, mesh%node, mesh%ele, jac_3d, FEM_elen,           &
     &      dxidxs%dx_nod, inod, ele_4_nod_s, neib_nod_s)
!
        nnod_near_nod_weight(inod) = nnod_near_1nod_weight
        call cal_filter_moms_each_nod_type(inod, mom_nod)
      end do
!
      call dealloc_iele_belonged(ele_4_nod_s)
      call dealloc_inod_next_node(neib_nod_s)
!
      end subroutine set_simple_filter
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_fluid_filter(file_name, mesh,               &
     &          jac_3d, FEM_elen, dxidxs, mom_nod)
!
      use t_filter_dxdxi
      use t_filter_moments
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
!
      character(len = kchara), intent(in) :: file_name
      type(mesh_geometry),       intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fluid_fileters(mesh, ele_4_nod_s, neib_nod_s)
!
      write(80+my_rank,*) ' Best condition for filter'
!
      i_exp_level_1nod_weight = maximum_neighbour
      do inod = inod_start_filter, inod_end_filter
        call set_simple_fl_filter_nod_by_nod                            &
     &     (file_name, mesh%node, mesh%ele, jac_3d, FEM_elen,           &
     &      dxidxs%dx_nod, inod, ele_4_nod_s, neib_nod_s, mom_nod)
      end do
!
      call dealloc_iele_belonged(ele_4_nod_s)
      call dealloc_inod_next_node(neib_nod_s)
!
      end subroutine set_simple_fluid_filter
!
! -----------------------------------------------------------------------
!
      end module cal_filter_func_node
