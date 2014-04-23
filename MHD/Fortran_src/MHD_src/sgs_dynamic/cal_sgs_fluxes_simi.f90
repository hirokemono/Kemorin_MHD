!
!      module cal_sgs_fluxes_simi
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_mf_simi(i_sgs, i_vect, i_vect_f, icm_sgs)
!      subroutine cal_sgs_hf_simi(i_sgs, ifield, ifield_f, icm_sgs)
!      subroutine cal_sgs_induct_t_simi(i_sgs, i_v, i_b,                &
!     &          i_fil_v, i_fil_b, icm_sgs)
!      subroutine cal_sgs_uxb_simi(i_sgs, i_v, i_b, i_fil_v, i_fil_b)
!
!      subroutine cal_sgs_uxb_2_ff_simi
!
!      subroutine cal_sgs_mf_simi_wide(i_sgs, i_vect, i_vect_f, icm_sgs)
!      subroutine cal_sgs_hf_simi_wide(i_sgs, ifield, ifield_f, icm_sgs)
!      subroutine cal_sgs_induct_t_simi_wide(i_sgs, i_v, i_b,           &
!     &          i_fil_v, i_fil_b, icm_sgs)
!      subroutine cal_sgs_uxb_simi_wide(i_sgs, i_v, i_b,                &
!     &          i_fil_v, i_fil_b)
!
      module cal_sgs_fluxes_simi
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_mf_simi(i_sgs, i_vect, i_vect_f, icm_sgs)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_filtering_tensors
!
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_vect_f
      integer (kind=kint), intent(in) :: icm_sgs
!
!  ----------   set filtered flux into array
!
       call cal_flux_tensor(i_sgs, i_vect, i_vect)
       call cal_filtered_sym_tensor(i_sgs, i_sgs)
!
!  ----------   substruct flux obtained by filterd values
!
       call cal_sgs_flux_tensor(i_sgs, i_vect_f, i_vect_f, icm_sgs)
!
      end subroutine cal_sgs_mf_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_hf_simi(i_sgs, ifield, ifield_f, icm_sgs)
!
      use m_node_phys_address
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_filtering_vectors
!
      integer (kind=kint), intent(in) :: i_sgs, ifield, ifield_f
      integer (kind=kint), intent(in) :: icm_sgs
!
!
      call cal_flux_vector(i_sgs, iphys%i_velo, ifield)
      call cal_filtered_vector(i_sgs, i_sgs)
!
      call cal_sgs_flux_vector(i_sgs, iphys%i_filter_velo, ifield_f,    &
     &    icm_sgs)
!
      end subroutine cal_sgs_hf_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_simi(i_sgs, i_v, i_b,                 &
     &          i_fil_v, i_fil_b, icm_sgs)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_filtering_vectors
!
      integer (kind=kint), intent(in) :: i_sgs, i_v, i_b
      integer (kind=kint), intent(in) :: i_fil_v, i_fil_b, icm_sgs
!
!  ----------   set filtered flux into array
!
       call cal_induction_tensor(i_sgs, i_b, i_v)
       call cal_filtered_vector(i_sgs, i_sgs)
!
!  ----------   substruct flux obtained by filterd values
!
       call subctract_induction_tensor(i_sgs, i_fil_b,                  &
     &     i_fil_v, icm_sgs)
!
      end subroutine cal_sgs_induct_t_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_simi(i_sgs, i_v, i_b, i_fil_v, i_fil_b)
!
      use cal_filtering_vectors
      use products_nodal_fields_smp
      use cal_similarity_terms
!
      integer (kind=kint), intent(in) :: i_sgs, i_v, i_b
      integer (kind=kint), intent(in) :: i_fil_v, i_fil_b
!
!
!$omp parallel
      call cal_phys_cross_product(i_v, i_b, i_sgs)
!$omp end parallel
!
      call cal_filtered_vector(i_sgs, i_sgs)
!
      call subctract_uxb_vector(i_sgs, i_fil_v, i_fil_b)
!
      end subroutine cal_sgs_uxb_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_ff_simi
!
      use m_node_phys_address
      use int_vol_similarity_uxb
!
!
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne)
!
      call sel_int_simi_vp_induct
!
      end subroutine cal_sgs_uxb_2_ff_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_mf_simi_wide(i_sgs, i_vect, i_vect_f, icm_sgs)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_w_filtering_tensors
!
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_vect_f
      integer (kind=kint), intent(in) :: icm_sgs
!
!  ----------   set filtered flux into array
!
       call cal_flux_tensor(i_sgs, i_vect, i_vect)
       call cal_w_filtered_sym_tensor(i_sgs, i_sgs)
!
!  ----------   substruct flux obtained by filterd values
!
       call cal_sgs_flux_tensor(i_sgs, i_vect_f, i_vect_f, icm_sgs)
!
      end subroutine cal_sgs_mf_simi_wide
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_hf_simi_wide(i_sgs, ifield, ifield_f, icm_sgs)
!
      use m_node_phys_address
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_w_filtering_vectors
!
      integer (kind=kint), intent(in) :: i_sgs, ifield, ifield_f
      integer (kind=kint), intent(in) :: icm_sgs
!
!
      call cal_flux_vector(i_sgs, iphys%i_velo, ifield)
      call cal_w_filtered_vector(i_sgs, i_sgs)
!
      call cal_sgs_flux_vector(i_sgs, iphys%i_filter_velo, ifield_f,    &
     &    icm_sgs)
!
      end subroutine cal_sgs_hf_simi_wide
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_simi_wide(i_sgs, i_v, i_b,            &
     &          i_fil_v, i_fil_b, icm_sgs)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_w_filtering_vectors
!
      integer (kind=kint), intent(in) :: i_sgs, i_v, i_b
      integer (kind=kint), intent(in) :: i_fil_v, i_fil_b, icm_sgs
!
!  ----------   set filtered flux into array
!
       call cal_induction_tensor(i_sgs, i_b, i_v)
       call cal_w_filtered_vector(i_sgs, i_sgs)
!
!  ----------   substruct flux obtained by filterd values
!
       call subctract_induction_tensor(i_sgs, i_fil_b,                  &
     &     i_fil_v, icm_sgs)
!
      end subroutine cal_sgs_induct_t_simi_wide
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_simi_wide(i_sgs, i_v, i_b,                 &
     &          i_fil_v, i_fil_b)
!
      use products_nodal_fields_smp
      use cal_similarity_terms
      use cal_w_filtering_vectors
!
      integer (kind=kint), intent(in) :: i_sgs, i_v, i_b
      integer (kind=kint), intent(in) :: i_fil_v, i_fil_b
!
!
!$omp parallel
      call cal_phys_cross_product(i_v, i_b, i_sgs)
!$omp end parallel
!
      call cal_w_filtered_vector(i_sgs, i_sgs)
!
      call subctract_uxb_vector(i_sgs, i_fil_v, i_fil_b)
!
      end subroutine cal_sgs_uxb_simi_wide
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_fluxes_simi
