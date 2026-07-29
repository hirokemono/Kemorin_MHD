!>@file   int_vol_rms_dynamic_gsmp_l.f90
!!@brief  module int_vol_rms_dynamic_gsmp_l
!!
!!@author H. Matsui
!!@date    Programmed by H.Matsui in Nov., 2008
!!
!>@brief  FEM integration for dynamic SGS model
!!
!!@verbatim
!!      subroutine s_int_vol_rms_dynamic_grpsmp_l                       &
!!     &         (numnod, numele, ie, interior_ele, n_tensor,           &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, an,                         &
!!     &          n_layer_d, n_item_layer_d, layer_stack,               &
!!     &          istack_item_layer_d_smp, item_layer, ntot_phys, d_nod,&
!!     &          i_sgs_simi, i_sgs_grad, i_sgs_grad_f,                 &
!!     &          ncomp_cor2, ave_l_smp, rms_l_smp, ave_l, rms_l,       &
!!     &          ave_w, rms_w)
!!        integer (kind = kint), intent(in) :: numele
!!        integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
!!        integer (kind = kint), intent(in) :: interior_ele(numele)
!!        integer (kind = kint), intent(in) :: n_tensor
!!        integer(kind = kint), intent(in) :: max_int_point
!!        integer(kind = kint), intent(in) :: maxtot_int_3d
!!        integer(kind = kint), intent(in) :: int_start3(max_int_point)
!!        real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!!        integer (kind=kint), intent(in) :: ntot_int_3d, n_int
!!        real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
!!        real(kind=kreal), intent(in) :: an(num_t_linear,ntot_int_3d)
!!        integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
!!        integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
!!        integer (kind = kint), intent(in)                             &
!!     &               :: istack_item_layer_d_smp(0:np_smp)
!!        integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!!        integer (kind = kint), intent(in) :: numnod, ntot_phys
!!        real(kind=kreal), intent(in) :: d_nod(numnod,ntot_phys)
!!        integer (kind = kint), intent(in) :: i_sgs_simi
!!        integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
!!        integer (kind = kint), intent(in) :: ncomp_cor2
!!        real(kind=kreal), intent(inout) :: ave_l_smp(np_smp,ncomp_cor2)
!!        real(kind=kreal), intent(inout) :: rms_l_smp(np_smp,ncomp_cor2)
!!        real(kind=kreal), intent(inout) :: ave_l(n_layer_d,ncomp_cor2)
!!        real(kind=kreal), intent(inout) :: rms_l(n_layer_d,ncomp_cor2)
!!        real(kind=kreal), intent(inout) :: ave_w(ncomp_cor2)
!!        real(kind=kreal), intent(inout) :: rms_w(ncomp_cor2)
!!@endverbatim
!
      module int_vol_rms_dynamic_gsmp_l
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_vol_rms_dynamic_grpsmp_l                         &
     &         (numnod, numele, ie, interior_ele, n_tensor,             &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, an,                           &
     &          n_layer_d, n_item_layer_d, layer_stack,                 &
     &          istack_item_layer_d_smp, item_layer, ntot_phys, d_nod,  &
     &          i_sgs_simi, i_sgs_grad, i_sgs_grad_f,                   &
     &          ncomp_cor2, ave_l_smp, rms_l_smp, ave_l, rms_l,         &
     &          ave_w, rms_w)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer (kind = kint), intent(in) :: n_tensor
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: an(num_t_linear,ntot_int_3d)
!
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in)                                 &
     &               :: istack_item_layer_d_smp(0:np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      integer (kind = kint), intent(in) :: numnod, ntot_phys
      real(kind=kreal), intent(in) :: d_nod(numnod,ntot_phys)
      integer (kind = kint), intent(in) :: i_sgs_simi
      integer (kind = kint), intent(in) :: i_sgs_grad, i_sgs_grad_f
!
      integer (kind = kint), intent(in) :: ncomp_cor2
      real(kind=kreal), intent(inout) :: ave_l_smp(np_smp,ncomp_cor2)
      real(kind=kreal), intent(inout) :: rms_l_smp(np_smp,ncomp_cor2)
      real(kind=kreal), intent(inout) :: ave_l(n_layer_d,ncomp_cor2)
      real(kind=kreal), intent(inout) :: rms_l(n_layer_d,ncomp_cor2)
      real(kind=kreal), intent(inout) :: ave_w(ncomp_cor2)
      real(kind=kreal), intent(inout) :: rms_w(ncomp_cor2)
!
      integer (kind = kint) :: inum, nd, iele, iele0
      integer (kind = kint) :: ist, ied, ist_num, ied_num
      integer (kind = kint) :: ii, ix, i_s, i_g, i_f, iproc
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
      ave_l =   0.0d0
      rms_l =   0.0d0
      ave_l_smp = 0.0d0
      rms_l_smp = 0.0d0
!
!$omp parallel do                                                       &
!$omp& private(ist_num,ied_num,inum,ist,ied,ii,ix,iele0,iele,nd,        &
!$omp&         i1,i2,i3,i4,i5,i6,i7,i8)
      do iproc = 1, np_smp
        ist_num = istack_item_layer_d_smp(iproc-1) + 1
        ied_num = istack_item_layer_d_smp(iproc  )
        do inum = ist_num, ied_num
          ist = layer_stack(inum-1) + 1
          ied = layer_stack(inum)
!
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
            do nd = 1, n_tensor
              i_s = i_sgs_simi +   nd-1
              i_g = i_sgs_grad +   nd-1
              i_f = i_sgs_grad_f + nd-1
!
!$cdir nodep
              do iele0 = ist, ied
                iele = item_layer(iele0)
!
                i1 = ie(iele,1)
                i2 = ie(iele,2)
                i3 = ie(iele,3)
                i4 = ie(iele,4)
                i5 = ie(iele,5)
                i6 = ie(iele,6)
                i7 = ie(iele,7)
                i8 = ie(iele,8)
!
                ave_l(inum,nd  ) = ave_l(inum,nd  )                     &
     &                + ( an(1, ix) * d_nod(i1, i_s)                    &
     &                  + an(2, ix) * d_nod(i2, i_s)                    &
     &                  + an(3, ix) * d_nod(i3, i_s)                    &
     &                  + an(4, ix) * d_nod(i4, i_s)                    &
     &                  + an(5, ix) * d_nod(i5, i_s)                    &
     &                  + an(6, ix) * d_nod(i6, i_s)                    &
     &                  + an(7, ix) * d_nod(i7, i_s)                    &
     &                  + an(8, ix) * d_nod(i8, i_s) )                  &
     &                 * dble(interior_ele(iele))                       &
     &                 * xjac(iele,ix) * owe3d(ix)
                ave_l(inum,nd+9) = ave_l(inum,nd+9)                     &
     &                + ( an(1, ix) * (d_nod(i1, i_f)-d_nod(i1, i_g))   &
     &                  + an(2, ix) * (d_nod(i2, i_f)-d_nod(i2, i_g))   &
     &                  + an(3, ix) * (d_nod(i3, i_f)-d_nod(i3, i_g))   &
     &                  + an(4, ix) * (d_nod(i4, i_f)-d_nod(i4, i_g))   &
     &                  + an(5, ix) * (d_nod(i5, i_f)-d_nod(i5, i_g))   &
     &                  + an(6, ix) * (d_nod(i6, i_f)-d_nod(i6, i_g))   &
     &                  + an(7, ix) * (d_nod(i7, i_f)-d_nod(i7, i_g))   &
     &                  + an(8, ix) * (d_nod(i8, i_f)-d_nod(i8, i_g)) ) &
     &                 * dble(interior_ele(iele))                       &
     &                 * xjac(iele,ix) * owe3d(ix)
!
                rms_l(inum,nd  ) = rms_l(inum,nd  )                     &
     &                + ( an(1, ix) * d_nod(i1, i_s)**2                 &
     &                  + an(2, ix) * d_nod(i2, i_s)**2                 &
     &                  + an(3, ix) * d_nod(i3, i_s)**2                 &
     &                  + an(4, ix) * d_nod(i4, i_s)**2                 &
     &                  + an(5, ix) * d_nod(i5, i_s)**2                 &
     &                  + an(6, ix) * d_nod(i6, i_s)**2                 &
     &                  + an(7, ix) * d_nod(i7, i_s)**2                 &
     &                  + an(8, ix) * d_nod(i8, i_s)**2 )               &
     &                 * dble(interior_ele(iele))                       &
     &                 * xjac(iele,ix) * owe3d(ix)
                rms_l(inum,nd+9) = rms_l(inum,nd+9)                     &
     &                + ( an(1, ix)                                     &
     &                    * ( d_nod(i1, i_f) - d_nod(i1, i_g) )**2      &
     &                  + an(2, ix)                                     &
     &                    * ( d_nod(i2, i_f) - d_nod(i2, i_g) )**2      &
     &                  + an(3, ix)                                     &
     &                    * ( d_nod(i3, i_f) - d_nod(i3, i_g) )**2      &
     &                  + an(4, ix)                                     &
     &                    * ( d_nod(i4, i_f) - d_nod(i4, i_g) )**2      &
     &                  + an(5, ix)                                     &
     &                    * ( d_nod(i5, i_f) - d_nod(i5, i_g) )**2      &
     &                  + an(6, ix)                                     &
     &                    * ( d_nod(i6, i_f) - d_nod(i6, i_g) )**2      &
     &                  + an(7, ix)                                     &
     &                    * ( d_nod(i7, i_f) - d_nod(i7, i_g) )**2      &
     &                  + an(8, ix)                                     &
     &                    * ( d_nod(i8, i_f) - d_nod(i8,i_g) )**2 )     &
     &                 * dble(interior_ele(iele))                       &
     &                 * xjac(iele,ix) * owe3d(ix)
!
              end do
!
            end do
          end do
!
          do nd = 1, n_tensor
            ave_l_smp(iproc,nd  ) = ave_l_smp(iproc,nd  )               &
     &                             + ave_l(inum,nd  )
            ave_l_smp(iproc,nd+9) = ave_l_smp(iproc,nd+9)               &
     &                             + ave_l(inum,nd+9)
            rms_l_smp(iproc,nd  ) = rms_l_smp(iproc,nd  )               &
     &                             + rms_l(inum,nd  )
            rms_l_smp(iproc,nd+9) = rms_l_smp(iproc,nd+9)               &
     &                             + rms_l(inum,nd+9)
          end do
!
        end do
      end do
!$omp end parallel do
!
      do nd = 1, n_tensor
        do iproc = 1, np_smp
          ave_w(nd  ) = ave_w(nd  ) + ave_l_smp(iproc,nd  )
          ave_w(nd+9) = ave_w(nd+9) + ave_l_smp(iproc,nd+9)
          rms_w(nd  ) = rms_w(nd  ) + rms_l_smp(iproc,nd  )
          rms_w(nd+9) = rms_w(nd+9) + rms_l_smp(iproc,nd+9)
        end do
      end do
!
      end subroutine s_int_vol_rms_dynamic_grpsmp_l
!
! ----------------------------------------------------------------------
!
      end module int_vol_rms_dynamic_gsmp_l
