!
!     module cal_stratification_by_temp
!
!        modified by H. Matsui in 2003
!        modified by H. Matsui on Aug., 2007
!
!      subroutine cal_stratified_layer
!      subroutine cal_stratified_layer_upw
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
!
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
      subroutine cal_stratified_layer
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_1st
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      call reset_sk6(n_scalar)
!
      do k2 = 1, ele1%nnod_4_ele
!
! --------- set position at each node in an element
       call position_2_each_element(k2, xe, radius_e)
       call scalar_phys_2_each_element(k2, iphys%i_gref_t, temp_e)
!
        call fem_skv_stratified_1st(iele_fl_smp_stack,                  &
     &      intg_point_t_evo, k2, temp_e, d_ele(1,iphys_ele%i_velo),    &
     &      xe, sk6)
!
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine cal_stratified_layer
!
!-----------------------------------------------------------------------
!
      subroutine cal_stratified_layer_upw
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_1st
!
       integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
!
! --------- set position at each node in an element
        call position_2_each_element(k2, xe, radius_e)
        call scalar_phys_2_each_element(k2, iphys%i_gref_t, temp_e)
!
        call fem_skv_stratified_upw_1st(iele_fl_smp_stack,              &
     &      intg_point_t_evo, k2, temp_e, d_ele(1,iphys_ele%i_velo),    &
     &      xe, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine cal_stratified_layer_upw
!
!-----------------------------------------------------------------------
!
      end module cal_stratification_by_temp
