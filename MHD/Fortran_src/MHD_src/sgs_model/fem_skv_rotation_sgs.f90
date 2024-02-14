!>@file   fem_skv_rotation_sgs.f90
!!        module fem_skv_rotation_sgs
!!
!>@author H. Matsui
!>@date  Programmed in July, 2005
!!       Modified in Aug., 2007
!!       Modified in Feb., 2024
!!
!>@brief FEM integeration for rotatin of SGS term
!!
!!@verbatim
!!      subroutine fem_skv_rot_sgs_pg(numele, nnod_4_e1, nnod_4_e2,     &
!!     &          np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,&
!!     &          int_start3, owe3d, n_int, k2, ntot_int_3d, xjac,      &
!!     &          an1, dnx1, dnx2, xmom_order2, elen_ele, ak_diff,      &
!!     &          vector_1, sk_v)
!!        integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
!!        integer(kind=kint), intent(in) :: np_smp, ntot_int_3d, n_int
!!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!!        integer(kind=kint), intent(in) :: k2
!!        integer(kind = kint), intent(in) :: max_int_point
!!        integer(kind = kint), intent(in) :: maxtot_int_3d
!!        integer(kind = kint), intent(in) :: int_start3(max_int_point)
!!        real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!!        real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
!!        real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
!!        real (kind=kreal), intent(in)                                 &
!!       &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
!!        real (kind=kreal), intent(in)                                 &
!!       &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!!        real(kind=kreal), intent(in) :: xmom_order2
!!        type(elen_ele_diffs_type), intent(in) :: elen_ele
!!        type(SGS_model_coefficient), intent(in) :: Cdiff
!!        real(kind=kreal), intent(in) :: vector_1(numele,3)
!!        real (kind=kreal), intent(inout)                              &
!!       &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!!@endverbatim
!
      module fem_skv_rotation_sgs
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use t_filter_elength
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_sgs_pg(numele, nnod_4_e1, nnod_4_e2,       &
     &          np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,  &
     &          int_start3, owe3d, n_int, k2, ntot_int_3d, xjac,        &
     &          an1, dnx1, dnx2, xmom_order2, elen_ele, ak_diff,        &
     &          vector_1, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d, n_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: k2
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real(kind=kreal), intent(in) :: xmom_order2
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      type(elen_ele_diffs_type), intent(in) ::   elen_ele
!      type(SGS_model_coefficient), intent(in) :: Cdiff
!
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind=kreal) :: div_x,  div_y,  div_z
      real(kind=kreal) :: grad_x, grad_y, grad_z
      integer(kind=kint) :: k1
      integer(kind=kint) :: iproc, iele, ii, ix
      integer(kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,div_x,div_y,div_z,  &
!$omp&                    grad_x,grad_y,grad_z)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = istart, iend
!
              grad_x = an1(k1,ix) * dnx2(iele,k2,ix,1)
              grad_y = an1(k1,ix) * dnx2(iele,k2,ix,2)
              grad_z = an1(k1,ix) * dnx2(iele,k2,ix,3)
!
!
              div_x = half * xmom_order2                                &
     &         * ( ( elen_ele%diff%df_x2(iele,1)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_xy(iele,1)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_zx(iele,1)*dnx1(iele,k1,ix,3) )   &
     &           * dnx2(iele,k2,ix,1)                                   &
     &           + ( elen_ele%diff%df_xy(iele,1)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_y2(iele,1)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_yz(iele,1)*dnx1(iele,k1,ix,3) )   &
     &           * dnx2(iele,k2,ix,2)                                   &
     &           + ( elen_ele%diff%df_zx(iele,1)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_yz(iele,1)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_z2(iele,1)* dnx1(iele,k1,ix,3) )  &
     &           * dnx2(iele,k2,ix,3) )
!
              div_y = half * xmom_order2                                &
     &         * ( ( elen_ele%diff%df_x2(iele,2)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_xy(iele,2)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_zx(iele,2)*dnx1(iele,k1,ix,3) )   &
     &           * dnx2(iele,k2,ix,1)                                   &
     &           + ( elen_ele%diff%df_xy(iele,2)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_y2(iele,2)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_yz(iele,2)*dnx1(iele,k1,ix,3) )   &
     &           * dnx2(iele,k2,ix,2)                                   &
     &           + ( elen_ele%diff%df_zx(iele,2)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_yz(iele,2)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_z2(iele,2)* dnx1(iele,k1,ix,3) )  &
     &           * dnx2(iele,k2,ix,3) )
!
              div_z = half * xmom_order2                                &
     &         * ( ( elen_ele%diff%df_x2(iele,3)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_xy(iele,3)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_zx(iele,3)*dnx1(iele,k1,ix,3) )   &
     &           * dnx2(iele,k2,ix,1)                                   &
     &           + ( elen_ele%diff%df_xy(iele,3)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_y2(iele,3)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_yz(iele,3)*dnx1(iele,k1,ix,3) )   &
     &           * dnx2(iele,k2,ix,2)                                   &
     &           + ( elen_ele%diff%df_zx(iele,3)*dnx1(iele,k1,ix,1)     &
     &             + elen_ele%diff%df_yz(iele,3)*dnx1(iele,k1,ix,2)     &
     &             + elen_ele%diff%df_z2(iele,3)* dnx1(iele,k1,ix,3) )  &
     &           * dnx2(iele,k2,ix,3) )
!
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &              + ( (grad_y + ak_diff(iele) * div_y )               &
     &                * vector_1(iele,3)                                &
     &               - ( grad_z + ak_diff(iele) * div_z )               &
     &                * vector_1(iele,2) ) * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &              + ( (grad_z + ak_diff(iele) * div_z )               &
     &                * vector_1(iele,1)                                &
     &               - ( grad_x + ak_diff(iele) * div_x )               &
     &                * vector_1(iele,3) ) * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &              + ( (grad_x + ak_diff(iele) * div_x )               &
     &                * vector_1(iele,2)                                &
     &               - ( grad_y + ak_diff(iele) * div_y )               &
     &                * vector_1(iele,1) ) * xjac(iele,ix)*owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_rot_sgs_pg
!
!-----------------------------------------------------------------------
!
      end module fem_skv_rotation_sgs
