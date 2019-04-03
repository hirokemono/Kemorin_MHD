!
!      module cal_filter_func_node
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine const_commute_filter_coefs(file_name, mesh,          &
!!     &          g_FEM, jac_3d, FEM_elen, ref_m, mom_nod,              &
!!     &          gfil_p, fil_coef, tmp_coef, whole_area, fil_mat)
!!      subroutine const_fluid_filter_coefs(file_name, gfil_p, mesh,    &
!!     &          g_FEM, jac_3d, FEM_elen, ref_m, fil_elist,            &
!!     &          fil_coef, tmp_coef, fluid_area, fil_mat)
!!        type(filter_area_flag), intent(inout) :: fluid_area
!!      subroutine set_simple_filter(file_name, mesh, g_FEM, jac_3d,    &
!!     &          FEM_elen, ref_m, gfil_p, dxidxs, mom_nod,             &
!!     &          fil_coef, tmp_coef, fil_mat)
!!      subroutine set_simple_fluid_filter(file_name, gfil_p, mesh,     &
!!     &          g_FEM, jac_3d, FEM_elen, ref_m, fil_elist, dxidxs,    &
!!     &          mom_nod, fil_coef, fil_mat)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(reference_moments), intent(in) :: ref_m
!!        type(element_list_4_filter), intent(in) :: fil_elist
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!
      module cal_filter_func_node
!
      use m_precision
!
      use m_constants
      use m_crs_matrix_4_filter
      use t_mesh_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_filter_elength
      use t_next_node_ele_4_node
      use t_reference_moments
      use t_filter_coefs
      use t_matrix_4_filter
      use t_element_list_4_filter
      use t_ctl_params_4_gen_filter
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
      subroutine const_commute_filter_coefs(file_name, mesh,            &
     &          g_FEM, jac_3d, FEM_elen, ref_m, mom_nod,                &
     &          gfil_p, fil_coef, tmp_coef, whole_area, fil_mat)
!
      use t_filter_moments
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
      use set_simple_filters
!
      character(len = kchara), intent(in) :: file_name
      type(mesh_geometry),       intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(filter_area_flag), intent(inout) :: whole_area
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: inod, ierr
!
!
      call init_4_cal_fileters(mesh, gfil_p, ele_4_nod_s, neib_nod_s,   &
     &    fil_coef, tmp_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!
      write(70+my_rank,*) ' Best condition for filter'
!
      do inod = gfil_p%inod_start_filter, gfil_p%inod_end_filter
        call const_filter_func_nod_by_nod(file_name, inod, gfil_p,      &
     &      mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen, ref_m,        &
     &      ele_4_nod_s, neib_nod_s, fil_coef, tmp_coef, whole_area,    &
     &      fil_mat, ierr)
!
        fil_coef%nnod_near_nod_w(inod) = fil_coef%nnod_4_1nod_w
        call cal_filter_moms_each_nod_type                              &
     &     (inod, ref_m, fil_coef, fil_mat, mom_nod)
      end do
!
      call dealloc_iele_belonged(ele_4_nod_s)
      call dealloc_inod_next_node(neib_nod_s)
!
      end subroutine const_commute_filter_coefs
!
! -----------------------------------------------------------------------
!
      subroutine const_fluid_filter_coefs(file_name, gfil_p, mesh,      &
     &          g_FEM, jac_3d, FEM_elen, ref_m, fil_elist,              &
     &          fil_coef, tmp_coef, fluid_area, fil_mat)
!
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
!
      character(len = kchara), intent(in) :: file_name
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(mesh_geometry),       intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(filter_area_flag), intent(inout) :: fluid_area
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: inod, ierr
!
!
      call init_4_cal_fluid_fileters                                    &
     &   (mesh, fil_elist, ele_4_nod_s, neib_nod_s)
!
      write(70+my_rank,*) ' Best condition for fluid filter'
!
      do inod = gfil_p%inod_start_filter, gfil_p%inod_end_filter
        call const_fluid_filter_nod_by_nod(file_name, inod, gfil_p,     &
     &      mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen, ref_m,        &
     &      ele_4_nod_s, neib_nod_s, fil_coef, tmp_coef, fluid_area,    &
     &      fil_mat, ierr)
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
      subroutine set_simple_filter(file_name, mesh, g_FEM, jac_3d,      &
     &          FEM_elen, ref_m, gfil_p, dxidxs, mom_nod,               &
     &          fil_coef, tmp_coef, fil_mat)
!
      use t_filter_dxdxi
      use t_filter_moments
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
      use set_simple_filters
!
      character(len = kchara), intent(in) :: file_name
      type(mesh_geometry),       intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fileters(mesh, gfil_p, ele_4_nod_s, neib_nod_s,   &
     &    fil_coef, tmp_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!
      fil_coef%ilevel_exp_1nod_w = gfil_p%maximum_neighbour
      do inod = gfil_p%inod_start_filter, gfil_p%inod_end_filter
        call set_simple_filter_nod_by_nod                               &
     &     (file_name, gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d,      &
     &      FEM_elen, dxidxs%dx_nod, ref_m, inod, ele_4_nod_s,          &
     &      neib_nod_s, fil_coef, fil_mat)
!
        fil_coef%nnod_near_nod_w(inod) = fil_coef%nnod_4_1nod_w
        call cal_filter_moms_each_nod_type                              &
     &     (inod, ref_m, fil_coef, fil_mat, mom_nod)
      end do
!
      call dealloc_iele_belonged(ele_4_nod_s)
      call dealloc_inod_next_node(neib_nod_s)
!
      end subroutine set_simple_filter
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_fluid_filter(file_name, gfil_p, mesh,       &
     &          g_FEM, jac_3d, FEM_elen, ref_m, fil_elist, dxidxs,      &
     &          mom_nod, fil_coef, fil_mat)
!
      use t_filter_dxdxi
      use t_filter_moments
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
!
      character(len = kchara), intent(in) :: file_name
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(mesh_geometry),       intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
      type(each_filter_coef), intent(inout) :: fil_coef
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fluid_fileters                                    &
     &   (mesh, fil_elist, ele_4_nod_s, neib_nod_s)
!
      write(80+my_rank,*) ' Best condition for filter'
!
      fil_coef%ilevel_exp_1nod_w = gfil_p%maximum_neighbour
      do inod = gfil_p%inod_start_filter, gfil_p%inod_end_filter
        call set_simple_fl_filter_nod_by_nod                            &
     &     (file_name, gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d,      &
     &      FEM_elen, dxidxs%dx_nod, ref_m, inod, ele_4_nod_s,          &
     &      neib_nod_s, mom_nod, fil_coef, fil_mat)
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
