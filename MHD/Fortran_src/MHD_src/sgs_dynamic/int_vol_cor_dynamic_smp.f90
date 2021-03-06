!int_vol_cor_dynamic_smp.f90
!     module int_vol_cor_dynamic_smp
!
!     Written by H. Matsui
!
!!      subroutine int_vol_layer_cor_l                                  &
!!     &        (numnod, numele, ie, interior_ele, n_tensor,            &
!!     &         max_int_point, maxtot_int_3d, int_start3, owe3d,       &
!!     &         ntot_int_3d, n_int, xjac, an,                          &
!!     &         n_layer_d, n_item_layer_d, layer_stack_smp, item_layer,&
!!     &         ave_s, ave_g, ntot_phys, d_nod,                        &
!!     &         i_sgs_simi, i_sgs_grad, i_sgs_grad_f,                  &
!!     &         ncomp_cor, ncomp_cor2, sig_l_smp, cor_l_smp,           &
!!     &         sig_l, cov_l, sig_w, cov_w)
!!      subroutine int_vol_layer_cor_q                                  &
!!     &        (numnod, numele, ie, interior_ele, n_tensor,            &
!!     &         max_int_point, maxtot_int_3d, int_start3, owe3d,       &
!!     &         ntot_int_3d, n_int, xjac, aw,                          &
!!     &         n_layer_d, n_item_layer_d, layer_stack_smp, item_layer,&
!!     &         ave_s, ave_g, ntot_phys, d_nod,                        &
!!     &         i_sgs_simi, i_sgs_grad, i_sgs_grad_f,                  &
!!     &         ncomp_cor, ncomp_cor2, sig_l_smp, cor_l_smp,           &
!!     &         sig_l, cov_l, sig_w, cov_w)
!
      module int_vol_cor_dynamic_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_layer_cor_l                                    &
     &        (numnod, numele, ie, interior_ele, n_tensor,              &
     &         max_int_point, maxtot_int_3d, int_start3, owe3d,         &
     &         ntot_int_3d, n_int, xjac, an,                            &
     &         n_layer_d, n_item_layer_d, layer_stack_smp, item_layer,  &
     &         ave_s, ave_g, ntot_phys, d_nod,                          &
     &         i_sgs_simi, i_sgs_grad, i_sgs_grad_f,                    &
     &         ncomp_cor, ncomp_cor2, sig_l_smp, cor_l_smp,             &
     &         sig_l, cov_l, sig_w, cov_w)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer (kind = kint), intent(in) :: n_tensor
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: an(num_t_linear,ntot_int_3d)
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(in) :: ave_s(n_layer_d,n_tensor)
      real(kind = kreal), intent(in) :: ave_g(n_layer_d,n_tensor)
!
      integer (kind = kint), intent(in) :: numnod, ntot_phys
      real(kind=kreal), intent(in) :: d_nod(numnod,ntot_phys)
      integer (kind = kint), intent(in) :: i_sgs_simi
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
!
      integer (kind = kint), intent(in) :: ncomp_cor, ncomp_cor2
      real(kind=kreal), intent(inout) :: sig_l_smp(np_smp,ncomp_cor2)
      real(kind=kreal), intent(inout) :: cor_l_smp(np_smp,ncomp_cor)
      real(kind=kreal), intent(inout) :: sig_l(n_layer_d,ncomp_cor2)
      real(kind=kreal), intent(inout) :: cov_l(n_layer_d,ncomp_cor)
      real(kind=kreal), intent(inout) :: sig_w(ncomp_cor2)
      real(kind=kreal), intent(inout) :: cov_w(ncomp_cor)
!
      integer (kind = kint) :: iproc, nd, iele, iele0, inum
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: is, ist, ied
      integer (kind = kint) :: i_s, i_g, i_f
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
      sig_l(1:n_layer_d,1:18) = 0.0d0
      cov_l(1:n_layer_d,1:9 ) = 0.0d0
      sig_w(1:18) = 0.0d0
      cov_w(1:9) =  0.0d0
!
      do inum = 1, n_layer_d
        sig_l_smp(1:np_smp,1:18) = 0.0d0
        cor_l_smp(1:np_smp,1:9) =  0.0d0
!
!$omp parallel do &
!$omp& private(nd,is,ist,ied,i_s,i_g,i_f,ii,ix,iele0,iele,&
!$omp&         i1,i2,i3,i4,i5,i6,i7,i8)
        do iproc = 1, np_smp
          is = (inum-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!
          do nd = 1, n_tensor
!
            i_s = i_sgs_simi +   nd-1
            i_g = i_sgs_grad +   nd-1
            i_f = i_sgs_grad_f + nd-1
!
            do ii= 1, n_int * n_int * n_int 
              ix = int_start3(n_int) + ii
!
!$cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
!
                i1 =  ie(iele, 1)
                i2 =  ie(iele, 2)
                i3 =  ie(iele, 3)
                i4 =  ie(iele, 4)
                i5 =  ie(iele, 5)
                i6 =  ie(iele, 6)
                i7 =  ie(iele, 7)
                i8 =  ie(iele, 8)
!
              sig_l_smp(iproc,nd  ) = sig_l_smp(iproc,nd  )             &
     &         + ( an(1, ix) * ( d_nod(i1, i_s) - ave_s(inum,nd) )**2   &
     &           + an(2, ix) * ( d_nod(i2, i_s) - ave_s(inum,nd) )**2   &
     &           + an(3, ix) * ( d_nod(i3, i_s) - ave_s(inum,nd) )**2   &
     &           + an(4, ix) * ( d_nod(i4, i_s) - ave_s(inum,nd) )**2   &
     &           + an(5, ix) * ( d_nod(i5, i_s) - ave_s(inum,nd) )**2   &
     &           + an(6, ix) * ( d_nod(i6, i_s) - ave_s(inum,nd) )**2   &
     &           + an(7, ix) * ( d_nod(i7, i_s) - ave_s(inum,nd) )**2   &
     &           + an(8, ix) * ( d_nod(i8, i_s) - ave_s(inum,nd) )**2 ) &
     &          * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
              sig_l_smp(iproc,nd+9) = sig_l_smp(iproc,nd+9)             &
     &              + ( an(1, ix) * ( d_nod(i1, i_f)                    &
     &                         - d_nod(i1, i_g) - ave_g(inum,nd) )**2   &
     &                + an(2, ix) * ( d_nod(i2, i_f)                    &
     &                         - d_nod(i2, i_g) - ave_g(inum,nd) )**2   &
     &                + an(3, ix) * ( d_nod(i3, i_f)                    &
     &                         - d_nod(i3, i_g) - ave_g(inum,nd) )**2   &
     &                + an(4, ix) * ( d_nod(i4, i_f)                    &
     &                         - d_nod(i4, i_g) - ave_g(inum,nd) )**2   &
     &                + an(5, ix) * ( d_nod(i5, i_f)                    &
     &                         - d_nod(i5, i_g) - ave_g(inum,nd) )**2   &
     &                + an(6, ix) * ( d_nod(i6, i_f)                    &
     &                         - d_nod(i6, i_g) - ave_g(inum,nd) )**2   &
     &                + an(7, ix) * ( d_nod(i7, i_f)                    &
     &                         - d_nod(i7, i_g) - ave_g(inum,nd) )**2   &
     &                + an(8, ix) * ( d_nod(i8, i_f)                    &
     &                         - d_nod(i8, i_g) - ave_g(inum,nd) )**2 ) &
     &               * dble(interior_ele(iele))                         &
     &               * xjac(iele,ix) * owe3d(ix)
!
              cor_l_smp(iproc,nd) =   cor_l_smp(iproc,nd)               &
     &              + ( an(1 ,ix) * ( d_nod(i1, i_f)                    &
     &                         - d_nod(i1, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i1, i_s) - ave_s(inum,nd) )      &
     &                + an(2 ,ix) * ( d_nod(i2, i_f)                    &
     &                         - d_nod(i2, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i2, i_s) - ave_s(inum,nd) )      &
     &                + an(3 ,ix) * ( d_nod(i3, i_f)                    &
     &                         - d_nod(i3, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i3, i_s) - ave_s(inum,nd) )      &
     &                + an(4 ,ix) * ( d_nod(i4, i_f)                    &
     &                         - d_nod(i4, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i4, i_s) - ave_s(inum,nd) )      &
     &                + an(5 ,ix) * ( d_nod(i5, i_f)                    &
     &                         - d_nod(i5, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i5, i_s) - ave_s(inum,nd) )      &
     &                + an(6 ,ix) * ( d_nod(i6, i_f)                    &
     &                         - d_nod(i6, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i6, i_s) - ave_s(inum,nd) )      &
     &                + an(7 ,ix) * ( d_nod(i7, i_f)                    &
     &                         - d_nod(i7, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i7, i_s) - ave_s(inum,nd) )      &
     &                + an(8 ,ix) * ( d_nod(i8, i_f)                    &
     &                         - d_nod(i8, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i8, i_s) - ave_s(inum,nd) ) )    &
     &               * dble(interior_ele(iele))                         &
     &               * xjac(iele,ix) * owe3d(ix)
!
              end do
!
            end do
          end do
        end do
!$omp end parallel do
!
        do nd = 1, n_tensor
          do iproc = 1, np_smp
            sig_l(inum,nd  ) = sig_l(inum,nd  ) + sig_l_smp(iproc,nd  )
            sig_l(inum,nd+9) = sig_l(inum,nd+9) + sig_l_smp(iproc,nd+9)
            cov_l(inum,nd  ) = cov_l(inum,nd  ) + cor_l_smp(iproc,nd  )
          end do
        end do
        do nd = 1, n_tensor
          sig_w(nd) =    sig_w(nd) +   sig_l(inum,nd  )
          sig_w(nd+9) =  sig_w(nd+9) + sig_l(inum,nd+9)
          cov_w(nd) =    cov_w(nd) +   cov_l(inum,nd  )
        end do
!
      end do
!
      end subroutine int_vol_layer_cor_l
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_layer_cor_q                                    &
     &        (numnod, numele, ie, interior_ele, n_tensor,              &
     &         max_int_point, maxtot_int_3d, int_start3, owe3d,         &
     &         ntot_int_3d, n_int, xjac, aw,                            &
     &         n_layer_d, n_item_layer_d, layer_stack_smp, item_layer,  &
     &         ave_s, ave_g, ntot_phys, d_nod,                          &
     &         i_sgs_simi, i_sgs_grad, i_sgs_grad_f,                    &
     &         ncomp_cor, ncomp_cor2, sig_l_smp, cor_l_smp,             &
     &         sig_l, cov_l, sig_w, cov_w)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_quad)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer (kind = kint), intent(in) :: n_tensor
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: aw(num_t_quad,ntot_int_3d)
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(in) :: ave_s(n_layer_d,n_tensor)
      real(kind = kreal), intent(in) :: ave_g(n_layer_d,n_tensor)
!
      integer (kind = kint), intent(in) :: numnod, ntot_phys
      real(kind=kreal), intent(in) :: d_nod(numnod,ntot_phys)
      integer (kind = kint), intent(in) :: i_sgs_simi
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
!
      integer (kind = kint), intent(in) :: ncomp_cor, ncomp_cor2
      real(kind=kreal), intent(inout) :: sig_l_smp(np_smp,ncomp_cor2)
      real(kind=kreal), intent(inout) :: cor_l_smp(np_smp,ncomp_cor)
      real(kind=kreal), intent(inout) :: sig_l(n_layer_d,ncomp_cor2)
      real(kind=kreal), intent(inout) :: cov_l(n_layer_d,ncomp_cor)
      real(kind=kreal), intent(inout) :: sig_w(ncomp_cor2)
      real(kind=kreal), intent(inout) :: cov_w(ncomp_cor)
!
      integer (kind = kint) :: iproc, nd, iele, iele0, inum
      integer (kind = kint) :: ii, ix, is, ist, ied
      integer (kind = kint) :: i_s, i_g, i_f
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer (kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
      sig_l(1:n_layer_d,1:18) = 0.0d0
      cov_l(1:n_layer_d,1:9 ) = 0.0d0
      sig_w(1:18) = 0.0d0
      cov_w(1:9) =  0.0d0
!
      do inum = 1, n_layer_d
        sig_l_smp(1:np_smp,1:18) = 0.0d0
        cor_l_smp(1:np_smp,1:9) =  0.0d0
!
!$omp parallel do &
!$omp& private(nd,is,ist,ied,i_s,i_g,i_f,ii,ix,iele0,iele,&
!$omp&         i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
!$omp&         i17,i18,i19,i20)
        do iproc = 1, np_smp
          is = (inum-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!
          do nd = 1, n_tensor
!
            i_s = i_sgs_simi +   nd-1
            i_g = i_sgs_grad +   nd-1
            i_f = i_sgs_grad_f + nd-1
!
            do ii= 1, n_int * n_int * n_int 
              ix = int_start3(n_int) + ii
!
!$cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
!
                i1 =  ie(iele, 1)
                i2 =  ie(iele, 2)
                i3 =  ie(iele, 3)
                i4 =  ie(iele, 4)
                i5 =  ie(iele, 5)
                i6 =  ie(iele, 6)
                i7 =  ie(iele, 7)
                i8 =  ie(iele, 8)
                i9 =  ie(iele, 9)
                i10 = ie(iele,10)
                i11 = ie(iele,11)
                i12 = ie(iele,12)
                i13 = ie(iele,13)
                i14 = ie(iele,14)
                i15 = ie(iele,15)
                i16 = ie(iele,16)
                i17 = ie(iele,17)
                i18 = ie(iele,18)
                i19 = ie(iele,19)
                i20 = ie(iele,20)
!
                sig_l_smp(iproc,nd  ) = sig_l_smp(iproc,nd  )           &
     &         + ( aw(1, ix) * ( d_nod(i1, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(2, ix) * ( d_nod(i2, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(3, ix) * ( d_nod(i3, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(4, ix) * ( d_nod(i4, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(5, ix) * ( d_nod(i5, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(6, ix) * ( d_nod(i6, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(7, ix) * ( d_nod(i7, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(8, ix) * ( d_nod(i8, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(9, ix) * ( d_nod(i9, i_s) - ave_s(inum,nd) )**2   &
     &           + aw(10,ix) * ( d_nod(i10,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(11,ix) * ( d_nod(i11,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(12,ix) * ( d_nod(i12,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(13,ix) * ( d_nod(i13,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(14,ix) * ( d_nod(i14,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(15,ix) * ( d_nod(i15,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(16,ix) * ( d_nod(i16,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(17,ix) * ( d_nod(i17,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(18,ix) * ( d_nod(i18,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(19,ix) * ( d_nod(i19,i_s) - ave_s(inum,nd) )**2   &
     &           + aw(20,ix) * ( d_nod(i20,i_s) - ave_s(inum,nd) )**2 ) &
     &          * dble(interior_ele(iele)) * xjac(iele,ix) * owe3d(ix)
!
                sig_l_smp(iproc,nd+9) = sig_l_smp(iproc,nd+9)           &
     &              + ( aw(1, ix) * ( d_nod(i1, i_f)                    &
     &                         - d_nod(i1, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(2, ix) * ( d_nod(i2, i_f)                    &
     &                         - d_nod(i2, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(3, ix) * ( d_nod(i3, i_f)                    &
     &                         - d_nod(i3, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(4, ix) * ( d_nod(i4, i_f)                    &
     &                         - d_nod(i4, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(5, ix) * ( d_nod(i5, i_f)                    &
     &                         - d_nod(i5, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(6, ix) * ( d_nod(i6, i_f)                    &
     &                         - d_nod(i6, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(7, ix) * ( d_nod(i7, i_f)                    &
     &                         - d_nod(i7, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(8, ix) * ( d_nod(i8, i_f)                    &
     &                         - d_nod(i8, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(9, ix) * ( d_nod(i9, i_f)                    &
     &                         - d_nod(i9, i_g) - ave_g(inum,nd) )**2   &
     &                + aw(10,ix) * ( d_nod(i10,i_f)                    &
     &                         - d_nod(i10,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(11,ix) * ( d_nod(i11,i_f)                    &
     &                         - d_nod(i11,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(12,ix) * ( d_nod(i12,i_f)                    &
     &                         - d_nod(i12,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(13,ix) * ( d_nod(i13,i_f)                    &
     &                         - d_nod(i13,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(14,ix) * ( d_nod(i14,i_f)                    &
     &                         - d_nod(i14,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(15,ix) * ( d_nod(i15,i_f)                    &
     &                         - d_nod(i15,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(16,ix) * ( d_nod(i16,i_f)                    &
     &                         - d_nod(i16,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(17,ix) * ( d_nod(i17,i_f)                    &
     &                         - d_nod(i17,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(18,ix) * ( d_nod(i18,i_f)                    &
     &                         - d_nod(i18,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(19,ix) * ( d_nod(i19,i_f)                    &
     &                         - d_nod(i19,i_g) - ave_g(inum,nd) )**2   &
     &                + aw(20,ix) * ( d_nod(i20,i_f)                    &
     &                         - d_nod(i20,i_g) - ave_g(inum,nd) )**2 ) &
     &               * dble(interior_ele(iele))                         &
     &               * xjac(iele,ix) * owe3d(ix)
!
                cor_l_smp(iproc,nd  ) = cor_l_smp(iproc,nd  )           &
     &              + ( aw(1 ,ix) * ( d_nod(i1, i_f)                    &
     &                         - d_nod(i1, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i1, i_s) - ave_s(inum,nd) )      &
     &                + aw(2 ,ix) * ( d_nod(i2, i_f)                    &
     &                         - d_nod(i2, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i2, i_s) - ave_s(inum,nd) )      &
     &                + aw(3 ,ix) * ( d_nod(i3, i_f)                    &
     &                         - d_nod(i3, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i3, i_s) - ave_s(inum,nd) )      &
     &                + aw(4 ,ix) * ( d_nod(i4, i_f)                    &
     &                         - d_nod(i4, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i4, i_s) - ave_s(inum,nd) )      &
     &                + aw(5 ,ix) * ( d_nod(i5, i_f)                    &
     &                         - d_nod(i5, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i5, i_s) - ave_s(inum,nd) )      &
     &                + aw(6 ,ix) * ( d_nod(i6, i_f)                    &
     &                         - d_nod(i6, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i6, i_s) - ave_s(inum,nd) )      &
     &                + aw(7 ,ix) * ( d_nod(i7, i_f)                    &
     &                         - d_nod(i7, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i7, i_s) - ave_s(inum,nd) )      &
     &                + aw(8 ,ix) * ( d_nod(i8, i_f)                    &
     &                         - d_nod(i8, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i8, i_s) - ave_s(inum,nd) )      &
     &                + aw(9 ,ix) * ( d_nod(i9, i_f)                    &
     &                         - d_nod(i9, i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i9, i_s) - ave_s(inum,nd) )      &
     &                + aw(10,ix) * ( d_nod(i10,i_f)                    &
     &                         - d_nod(i10,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i10,i_s) - ave_s(inum,nd) )      &
     &                + aw(11,ix) * ( d_nod(i11,i_f)                    &
     &                         - d_nod(i11,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i11,i_s) - ave_s(inum,nd) )      &
     &                + aw(12,ix) * ( d_nod(i12,i_f)                    &
     &                         - d_nod(i12,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i12,i_s) - ave_s(inum,nd) )      &
     &                + aw(13,ix) * ( d_nod(i13,i_f)                    &
     &                         - d_nod(i13,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i13,i_s) - ave_s(inum,nd) )      &
     &                + aw(14,ix) * ( d_nod(i14,i_f)                    &
     &                         - d_nod(i14,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i14,i_s) - ave_s(inum,nd) )      &
     &                + aw(15,ix) * ( d_nod(i15,i_f)                    &
     &                         - d_nod(i15,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i15,i_s) - ave_s(inum,nd) )      &
     &                + aw(16,ix) * ( d_nod(i16,i_f)                    &
     &                         - d_nod(i16,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i16,i_s) - ave_s(inum,nd) )      &
     &                + aw(17,ix) * ( d_nod(i17,i_f)                    &
     &                         - d_nod(i17,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i17,i_s) - ave_s(inum,nd) )      &
     &                + aw(18,ix) * ( d_nod(i18,i_f)                    &
     &                         - d_nod(i18,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i18,i_s) - ave_s(inum,nd) )      &
     &                + aw(19,ix) * ( d_nod(i19,i_f)                    &
     &                         - d_nod(i19,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i19,i_s) - ave_s(inum,nd) )      &
     &                + aw(20,ix) * ( d_nod(i20,i_f)                    &
     &                         - d_nod(i20,i_g) - ave_g(inum,nd) )      &
     &                       * ( d_nod(i20,i_s) - ave_s(inum,nd) ) )    &
     &               * dble(interior_ele(iele))                         &
     &               * xjac(iele,ix) * owe3d(ix)
!
              end do
!
            end do
          end do
        end do
!$omp end parallel do
!
        do nd = 1, n_tensor
          do iproc = 1, np_smp
            sig_l(inum,nd  ) = sig_l(inum,nd  ) + sig_l_smp(iproc,nd  )
            sig_l(inum,nd+9) = sig_l(inum,nd+9) + sig_l_smp(iproc,nd+9)
            cov_l(inum,nd  ) = cov_l(inum,nd  ) + cor_l_smp(iproc,nd  )
          end do
        end do
        do nd = 1, n_tensor
          sig_w(nd) =    sig_w(nd) +   sig_l(inum,nd  )
          sig_w(nd+9) =  sig_w(nd+9) + sig_l(inum,nd+9)
          cov_w(nd) =    cov_w(nd) +   cov_l(inum,nd  )
        end do
!
      end do
!
      end subroutine int_vol_layer_cor_q
!
!-----------------------------------------------------------------------
!
      end module int_vol_cor_dynamic_smp
