!
!      module int_vol_initial_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!      subroutine int_vol_initial_velo
!      subroutine int_vol_initial_vect_p
!      subroutine int_vol_initial_magne
!      subroutine int_vol_initial_temp
!      subroutine int_vol_initial_part_temp
!      subroutine int_vol_initial_d_scalar
!
      module int_vol_initial_MHD
!
      use m_precision
!
      use m_phys_constants
!
      implicit none
!
      private :: int_vol_initial_scalar
      private :: int_vol_initial_vector
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_velo
!
       use m_geometry_data_MHD
       use m_node_phys_address
       use m_physical_property
       use m_finite_element_matrix
!
       ff_m_smp = 0.0d0
       if (coef_velo.gt.0.0d0) then
         call int_vol_initial_vector(iele_fl_smp_stack, iphys%i_velo)
       end if
!
      end subroutine int_vol_initial_velo
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_vect_p
!
       use m_geometry_data_MHD
       use m_node_phys_address
       use m_physical_property
       use m_finite_element_matrix
!
       ff_m_smp = 0.0d0
       if (coef_magne.gt.0.0d0) then
         call int_vol_initial_vector(iele_cd_smp_stack, iphys%i_vecp)
       end if
!
      end subroutine int_vol_initial_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_magne
!
       use m_geometry_data_MHD
       use m_node_phys_address
       use m_physical_property
       use m_finite_element_matrix
!
       ff_m_smp = 0.0d0
       if (coef_magne.gt.0.0d0) then
         call int_vol_initial_vector(iele_cd_smp_stack, iphys%i_magne)
       end if
!
      end subroutine int_vol_initial_magne
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_temp
!
       use m_geometry_data_MHD
       use m_node_phys_address
       use m_physical_property
       use m_finite_element_matrix
!
       ff_m_smp = 0.0d0
       if (coef_temp.gt.0.0d0) then
         call int_vol_initial_scalar(iele_fl_smp_stack, iphys%i_temp)
       end if
!
      end subroutine int_vol_initial_temp
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_part_temp
!
       use m_geometry_data_MHD
       use m_node_phys_address
       use m_physical_property
       use m_finite_element_matrix
!
       ff_m_smp = 0.0d0
       if (coef_temp.gt.0.0d0) then
         call int_vol_initial_scalar(iele_fl_smp_stack,                 &
     &       iphys%i_par_temp)
       end if
!
      end subroutine int_vol_initial_part_temp
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_d_scalar
!
       use m_geometry_data_MHD
       use m_node_phys_address
       use m_physical_property
       use m_finite_element_matrix
!
       ff_m_smp = 0.0d0
       if (coef_scalar.gt.0.0d0) then
         call int_vol_initial_scalar(iele_fl_smp_stack, iphys%i_light)
       end if
!
      end subroutine int_vol_initial_d_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_scalar(iele_fsmp_stack, i_field)
!
      use m_control_parameter
      use m_geometry_parameter
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nodal_field_1st
!
      integer(kind = kint), intent(in) :: i_field
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for phsical values
      do k2=1, nnod_4_ele
        call scalar_phys_2_each_element(k2, i_field, phi_e)
        call fem_skv_scalar_1st(iele_fsmp_stack, intg_point_t_evo, k2,  &
     &      phi_e, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_m_smp, sk6)
!
      end subroutine int_vol_initial_scalar
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_vector(iele_fsmp_stack, i_field)
!
      use m_control_parameter
      use m_geometry_parameter
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nodal_field_1st
!
      integer(kind = kint), intent(in) :: i_field
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for phsical values
      do k2=1, nnod_4_ele
        call vector_phys_2_each_element(k2, i_field, vect_e)
        call fem_skv_vector_1st(iele_fsmp_stack, intg_point_t_evo, k2,  &
     &      vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_m_smp, sk6)
!
      end subroutine int_vol_initial_vector
!
!-----------------------------------------------------------------------
!
      end module int_vol_initial_MHD
