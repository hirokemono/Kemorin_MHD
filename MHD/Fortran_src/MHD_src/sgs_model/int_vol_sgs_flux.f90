!
!      module int_vol_sgs_flux
!
!        Written by H.Matsui   on July 2005
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on July, 2007
!        modified by H. Matsui on April, 2012
!
!      subroutine sel_int_vol_sgs_flux(iflag_4_supg, i_filter, numdir,  &
!     &           i_field, id_dx, sk_v)
!
      module int_vol_sgs_flux
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_geometry_data
!
      implicit none
!
      private :: int_vol_sgs_flux_pg, int_vol_sgs_flux_upwind
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_vol_sgs_flux(iflag_4_supg, i_filter, numdir,   &
     &           i_field, id_dx, sk_v)
!
      use m_element_phys_address
      use m_element_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer (kind = kint), intent(in) :: id_dx, i_filter
      integer (kind = kint), intent(in) :: numdir, i_field
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_sgs_flux_upwind(i_filter, numdir, i_field, id_dx,  &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, d_ele, sk_v)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_sgs_flux_upwind(i_filter, numdir, i_field, id_dx,  &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, d_ele, sk_v)
      else
        call int_vol_sgs_flux_pg(i_filter, numdir,                      &
     &      i_field, id_dx, sk_v)
      end if
!
      end subroutine sel_int_vol_sgs_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_flux_pg(i_filter, numdir, i_field, id_dx,  &
     &          sk_v)
!
      use m_geometry_data_MHD
      use m_int_vol_data
      use m_SGS_model_coefs
!
      use fem_skv_sgs_flux_1st
      use nodal_fld_2_each_ele_1st
!
      integer (kind = kint), intent(in) :: id_dx, i_filter
      integer (kind = kint), intent(in) :: numdir, i_field
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint) :: icomp, nd, ndx, nd2, nd_t
      integer (kind = kint) :: k2, id_dvx2
!
!
      do nd = 1, numdir
        icomp = i_field + nd - 1
!
        do ndx = 1, 4-nd
          nd2 = nd + ndx - 1
          id_dvx2 = id_dx + 3*(nd2-1)
          nd_t = lst_sim_t(nd) + ndx
!
! -------- loop for shape function for the phsical values
          do k2 = 1, ele1%nnod_4_ele
            call scalar_phys_2_each_element(k2, icomp, phi_e)
            call fem_skv_sgs_flux_pg_1(iele_fl_smp_stack,               &
     &          intg_point_t_evo, k2, phi_e, dvx(1,id_dvx2), i_filter,  &
     &          nd_t, sk_v)
          end do
!
        end do
      end do
!
      end subroutine int_vol_sgs_flux_pg
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_flux_upwind(i_filter, numdir,              &
     &          i_field, id_dx, ncomp_ele, ie_upw, d_ele, sk_v)
!
      use m_geometry_data_MHD
      use m_int_vol_data
      use m_SGS_model_coefs
!
      use fem_skv_sgs_flux_1st
      use nodal_fld_2_each_ele_1st
!
      integer (kind = kint), intent(in) :: i_filter, id_dx
      integer (kind = kint), intent(in) :: numdir, i_field
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      integer (kind = kint) :: icomp, nd, ndx, nd2, nd_t
      integer (kind = kint) :: k2, id_dvx2
!
!  ---------  set number of integral points
!
      do nd = 1, numdir
        icomp = i_field + nd - 1
!
        do ndx = 1, 4-nd
          nd2 = nd + ndx - 1
          id_dvx2 = id_dx + 3*(nd2-1)
          nd_t = lst_sim_t(nd) + ndx
!
! -------- loop for shape function for the phsical values
!
          do k2 = 1, ele1%nnod_4_ele
            call scalar_phys_2_each_element(k2, icomp, phi_e)
            call fem_skv_sgs_flux_upw_1(iele_fl_smp_stack,              &
     &          intg_point_t_evo, k2, phi_e, d_ele(1,ie_upw),           &
     &          dvx(1,id_dvx2), i_filter, nd_t, sk_v)
          end do
        end do
      end do
!
      end subroutine int_vol_sgs_flux_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_flux
