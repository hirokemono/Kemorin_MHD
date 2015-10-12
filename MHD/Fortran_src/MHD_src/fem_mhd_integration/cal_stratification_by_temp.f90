!
!     module cal_stratification_by_temp
!
!        modified by H. Matsui in 2003
!        modified by H. Matsui on Aug., 2007
!
!      subroutine cal_stratified_layer(ncomp_ele, iele_velo, d_ele)
!      subroutine cal_stratified_layer_upw(ncomp_ele, iele_velo, d_ele)
!
!*****************************************************
!
!      subroutine for set strtified layer
!       define... see Takehiro & Rister (2001)
!
!       Written by H. Matsui
!
!*****************************************************
!
!*****************************************************
!
!   Paramters
!      stratified_sigma:   intensity of stratification
!      stratified_width:   width of stratified layer
!      stratified_outer_r: radius of outer boundary of stratified layer
!
!*****************************************************
!
      module cal_stratification_by_temp
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_node_phys_address
!
      use m_sorted_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
      use nodal_fld_2_each_ele_1st
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_stratified_layer(ncomp_ele, iele_velo, d_ele)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      do k2 = 1, ele1%nnod_4_ele
!
! --------- set position at each node in an element
       call position_2_each_element                                     &
     &    (k2, mhd_fem1_wk%xx_e, mhd_fem1_wk%rr_e)
       call scalar_phys_2_each_element                                  &
     &    (k2, iphys%i_gref_t, fem1_wk%scalar_1)
!
        call fem_skv_stratified_galerkin(iele_fl_smp_stack,             &
     &      intg_point_t_evo, k2, fem1_wk%scalar_1, d_ele(1,iele_velo), &
     &      mhd_fem1_wk%xx_e, ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine cal_stratified_layer
!
!-----------------------------------------------------------------------
!
      subroutine cal_stratified_layer_upw(ncomp_ele, iele_velo, d_ele)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
       integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
!
! --------- set position at each node in an element
        call position_2_each_element                                    &
     &     (k2, mhd_fem1_wk%xx_e, mhd_fem1_wk%rr_e)
        call scalar_phys_2_each_element                                 &
     &     (k2, iphys%i_gref_t, fem1_wk%scalar_1)
!
        call fem_skv_stratified_upwind(iele_fl_smp_stack,               &
     &      intg_point_t_evo, k2, fem1_wk%scalar_1, d_ele(1,iele_velo), &
     &      mhd_fem1_wk%xx_e, ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine cal_stratified_layer_upw
!
!-----------------------------------------------------------------------
!
      end module cal_stratification_by_temp
