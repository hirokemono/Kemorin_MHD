!
!     module int_vol_sgs_uxb
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Apr., 2012
!
!      subroutine sel_int_vol_sgs_uxb(i_filter, i_field, id_dx)
!
      module int_vol_sgs_uxb
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_geometry_data
!
      implicit none
!
      private :: int_vol_sgs_uxb_upm, int_vol_sgs_uxb_pg
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_int_vol_sgs_uxb(i_filter, i_field, id_dx, sk_v)
!
      use m_element_phys_address
      use m_element_phys_data
!
      integer (kind=kint), intent(in) :: i_field, i_filter
      integer (kind=kint), intent(in) :: id_dx
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      if (iflag_mag_supg .eq. id_turn_ON) then
        call int_vol_sgs_uxb_upm(i_filter, i_field, id_dx,              &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, d_ele, sk_v)
      else
        call int_vol_sgs_uxb_pg(i_filter, i_field, id_dx, sk_v)
      end if
!
      end subroutine sel_int_vol_sgs_uxb
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_uxb_pg(i_filter, i_field, id_dx, sk_v)
!
      use m_SGS_model_coefs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      use fem_skv_sgs_flux_1st
      use nodal_fld_2_each_ele_1st
!
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: id_dx, i_filter
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer(kind = kint) :: nd, k2
!
!
      do nd = 1, n_vector
!
! -------- loop for shape function for the phsical values
!
        do k2 = 1, ele1%nnod_4_ele
!
! --------- set magnetic field at each node in an element
!
          call vector_phys_2_each_element(k2, i_field, vect_e )
          call fem_skv_sgs_uxb_pg_1(iele_cd_smp_stack,                  &
     &        intg_point_t_evo, k2, vect_e, dvx(1,id_dx),               &
     &        i_filter, nd, sk_v)
        end do
      end do
!
      end subroutine int_vol_sgs_uxb_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_uxb_upm(i_filter, i_field, id_dx,          &
     &          ncomp_ele, i_magne, d_ele, sk_v)
!
      use m_SGS_model_coefs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      use fem_skv_sgs_flux_1st
      use nodal_fld_2_each_ele_1st
!
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: id_dx, i_filter
!
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer(kind = kint) :: nd, k2
!
!
      do nd = 1, n_vector
        do k2 = 1, ele1%nnod_4_ele
!
          call vector_phys_2_each_element(k2, i_field, vect_e)
          call fem_skv_sgs_uxb_upw_1(iele_cd_smp_stack,                 &
     &        intg_point_t_evo, k2, vect_e, d_ele(1,i_magne),           &
     &        dvx(1,id_dx), i_filter, nd, sk_v)
        end do
      end do
!
      end subroutine int_vol_sgs_uxb_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_uxb
