!
!     module int_vol_sgs_induct_t
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Apr., 2012
!
!      subroutine sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,    &
!     &           ifield_v, ifield_b, sk_v)
!
      module int_vol_sgs_induct_t
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_int_vol_data
!
      implicit none
!
      private :: int_vol_sgs_induct_t_pg, int_vol_sgs_induct_t_upm
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,     &
     &           ifield_v, ifield_b, sk_v)
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
      integer (kind = kint), intent(in) :: ie_dvx, ie_dbx
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_sgs_induct_t_upm(i_filter, ie_dvx, ie_dbx,         &
     &      ifield_v, ifield_b, sk_v)
      else
        call int_vol_sgs_induct_t_pg(i_filter, ie_dvx, ie_dbx,          &
     &      ifield_v, ifield_b, sk_v)
      end if
!
      end subroutine sel_int_vol_sgs_induct_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induct_t_pg(i_filter, ie_dvx, ie_dbx,      &
     &           ifield_v, ifield_b, sk_v)
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_sgs_flux_1st
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
      integer (kind = kint), intent(in) :: ie_dvx, ie_dbx
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint) :: nd, nd2, nd3
      integer (kind = kint) :: icomp_v, icomp_b
      integer (kind = kint) :: id_dvx2, id_dbx2
      integer (kind = kint) :: k2
!
!
      do nd = 1, n_asym_tensor
        nd2 = ithree - mod((nd+ione),itwo)
        nd3 = ithree - mod( nd,      ithree)
        id_dvx2 = ie_dvx + 3*(nd2-1)
        id_dbx2 = ie_dbx + 3*(nd2-1)
        icomp_v = ifield_v + nd3 - 1
        icomp_b = ifield_b + nd3 - 1
!
! -------- loop for shape function for the phsical values
!
        do k2 = 1, ele1%nnod_4_ele
!
! --------- set temperature at each node in an element
!
          call scalar_phys_2_each_element(k2, icomp_b, vect_e(1,1) )
          call scalar_phys_2_each_element(k2, icomp_v, vect_e(1,2) )
!
          call fem_skv_sgs_induct_t_pg_1(iele_cd_smp_stack,             &
     &        intg_point_t_evo, k2, vect_e, dvx(1,id_dvx2),             &
     &        dvx(1,id_dbx2),  i_filter, nd, sk_v)
        end do
      end do
!
      end subroutine int_vol_sgs_induct_t_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induct_t_upm(i_filter, ie_dvx, ie_dbx,     &
     &           ifield_v, ifield_b, sk_v)
!
      use m_element_phys_address
      use m_element_phys_data
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_sgs_flux_1st
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
      integer (kind = kint), intent(in) :: ie_dvx, ie_dbx
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint) :: nd, nd2, nd3
      integer (kind = kint) :: icomp_v, icomp_b
      integer (kind = kint) :: id_dvx2, id_dbx2
      integer (kind = kint) :: k2
!
!
      do nd = 1, n_asym_tensor
        nd2 = ithree - mod((nd+ione),itwo)
        nd3 = ithree - mod( nd,      ithree)
        id_dvx2 = ie_dvx + 3*(nd2-1)
        id_dbx2 = ie_dbx + 3*(nd2-1)
        icomp_v = ifield_v + nd3 - 1
        icomp_b = ifield_b + nd3 - 1
!
        do k2 = 1, ele1%nnod_4_ele
!
          call scalar_phys_2_each_element(k2, icomp_v, vect_e(1,1) )
          call scalar_phys_2_each_element(k2, icomp_b, vect_e(1,2) )
!
          call fem_skv_sgs_induct_t_upw_1(iele_cd_smp_stack,            &
     &        intg_point_t_evo, k2, vect_e, d_ele(1,iphys_ele%i_magne), &
     &        dvx(1,id_dvx2), dvx(1,id_dbx2), i_filter, nd, sk_v)
!
        end do
      end do
!
      end subroutine int_vol_sgs_induct_t_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_induct_t
