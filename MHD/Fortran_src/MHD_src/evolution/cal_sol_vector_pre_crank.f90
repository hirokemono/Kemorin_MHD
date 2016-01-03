!
!      module cal_sol_vector_pre_crank
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_sol_velo_pre_linear(node, iphys, nod_fld)
!!      subroutine cal_sol_temp_linear(node, iphys, nod_fld)
!!      subroutine cal_sol_par_temp_linear(node, iphys, nod_fld)
!!      subroutine cal_sol_vect_p_pre_linear(node, iphys, nod_fld)
!!      subroutine cal_sol_magne_pre_linear(node, iphys, nod_fld)
!!      subroutine cal_sol_d_scalar_linear(node, iphys, nod_fld)
!!
!!      subroutine cal_vector_pre_consist                               &
!!     &          (node, coef_field, ff_nl, numdir, if_pre, nod_fld, ff)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_sol_vector_pre_crank
!
      use m_precision
!
      use m_phys_constants
      use m_finite_element_matrix
      use cal_sol_field_explicit
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_linear(node, iphys, nod_fld)
!
      use m_int_vol_data
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff, nod_fld%ntot_phys,       &
     &    n_vector, iphys%i_velo, iphys%i_pre_mom, nod_fld%d_fld,       &
     &    f1_l%ff)
!
      end subroutine cal_sol_velo_pre_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_linear(node, iphys, nod_fld)
!
      use m_int_vol_data
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff, nod_fld%ntot_phys,       &
     &    n_scalar, iphys%i_temp, iphys%i_pre_heat, nod_fld%d_fld,      &
     &    f1_l%ff)
!
      end subroutine cal_sol_temp_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_par_temp_linear(node, iphys, nod_fld)
!
      use m_int_vol_data
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff, nod_fld%ntot_phys,       &
     &    n_scalar, iphys%i_par_temp, iphys%i_pre_heat, nod_fld%d_fld,  &
     &    f1_l%ff)
!
      end subroutine cal_sol_par_temp_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_linear(node, iphys, nod_fld)
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_conduct_linear(node%numnod,                      &
     &    node%istack_internal_smp, conduct1%istack_inter_fld_smp,      &
     &    conduct1%numnod_fld, conduct1%inod_fld,                       &
     &    mhd_fem1_wk%mlump_cd%ml_o, f1_nl%ff,                          &
     &    nod_fld%ntot_phys, n_vector, iphys%i_vecp, iphys%i_pre_uxb,   &
     &    nod_fld%d_fld, f1_l%ff)
!
      end subroutine cal_sol_vect_p_pre_linear
!
! -----------------------------------------------------------------------!
      subroutine cal_sol_magne_pre_linear(node, iphys, nod_fld)
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_conduct_linear(node%numnod,                      &
     &    node%istack_internal_smp, conduct1%istack_inter_fld_smp,      &
     &    conduct1%numnod_fld, conduct1%inod_fld,                       &
     &    mhd_fem1_wk%mlump_cd%ml_o, f1_nl%ff,                          &
     &    nod_fld%ntot_phys, n_vector, iphys%i_magne, iphys%i_pre_uxb,  &
     &    nod_fld%d_fld, f1_l%ff)
!
      end subroutine cal_sol_magne_pre_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_linear(node, iphys, nod_fld)
!
      use m_int_vol_data
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_fluid_linear(node%numnod, node%istack_nod_smp,   &
     &    mhd_fem1_wk%mlump_fl%ml_o, f1_nl%ff, nod_fld%ntot_phys,       &
     &    n_scalar, iphys%i_light, iphys%i_pre_composit,                &
     &    nod_fld%d_fld, f1_l%ff)
!
      end subroutine cal_sol_d_scalar_linear
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vector_pre_consist                                 &
     &          (node, coef_field, ff_nl, numdir, if_pre, nod_fld, ff)
!
      use m_element_id_4_node
      use m_int_vol_data
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: coef_field
!
      integer (kind = kint), intent(in) :: numdir, if_pre
      real(kind = kreal), intent(in) :: ff_nl(node%numnod,3)
      real(kind = kreal), intent(inout) :: ff(node%numnod,3)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node%numnod, node%istack_internal_smp, ff_nl,                 &
     &    nod_fld%ntot_phys, numdir, if_pre, nod_fld%d_fld, ff)
!
      if (coef_field.gt.0.0d0) then
        call cal_ff_smp_2_ff(node, rhs_tbl1, numdir,                    &
     &      mhd_fem1_wk%ff_m_smp, ff)
      end if
!
      end subroutine cal_vector_pre_consist
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_pre_crank
