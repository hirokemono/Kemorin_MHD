!
!      module cal_coriolis_mat33
!
!     programmed by H.Matsui
!     modified by H. Matsui on Nov., 2008
!
!!      subroutine cal_lumped_coriolis_matrix                           &
!!     &         (numnod, numnod_fluid, inod_fluid, OLDtoNEW,           &
!!     &          coef_cor, angular, dt, coef_imp, ml_o,                &
!!     &          num_mat, aiccg33)
!!      subroutine cal_consist_coriolis_matrix                          &
!!     &         (np_smp, numele, nnod_4_e1, nnod_4_e2,                 &
!!     &          inod_ele_max, num_sort_smp, nod_stack_smp,            &
!!     &          iele_sort_smp, iconn_sort_smp, idx_4_mat, k2,         &
!!     &          coef_cor, angular, dt, coef_imp, sk_v,                &
!!     &          num_mat, aiccg33)
!
      module cal_coriolis_mat33
!
      use m_precision
!
      use m_constants
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_lumped_coriolis_matrix                             &
     &         (numnod, numnod_fluid, inod_fluid, OLDtoNEW,             &
     &          coef_cor, angular, dt, coef_imp, ml_o,                  &
     &          num_mat, aiccg33)
!
      integer(kind = kint), intent(in) :: numnod, numnod_fluid
      integer(kind = kint), intent(in) :: inod_fluid(numnod_fluid)
!
      integer(kind = kint), intent(in) :: OLDtoNEW(numnod)
!
      real(kind = kreal), intent(in) :: ml_o(numnod)
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: angular(3)
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: num_mat
      real(kind = kreal), intent(inout) :: aiccg33(-8:num_mat)
!
      integer(kind = kint) :: inum, inod, in
      real(kind = kreal) :: coef
!
      coef = coef_imp * dt * coef_cor
!$omp parallel do private(inum,inod,in)
      do inum = 1, numnod_fluid
        inod = inod_fluid(inum)
        in = OLDtoNEW(inod)
!        aiccg33(in*9-8) = aiccg33(in*9-8)
        aiccg33(in*9-7) = aiccg33(in*9-7)                         &
     &                      - coef * angular(3) * ml_o(inod)
        aiccg33(in*9-6) = aiccg33(in*9-6)                         &
     &                      + coef * angular(2) * ml_o(inod)
!
        aiccg33(in*9-5) = aiccg33(in*9-5)                         &
     &                      + coef * angular(3) * ml_o(inod)
!        aiccg33(in*9-4) = aiccg33(in*9-4)
        aiccg33(in*9-3) = aiccg33(in*9-3)                         &
     &                      - coef * angular(1) * ml_o(inod)
!
        aiccg33(in*9-2) = aiccg33(in*9-2)                         &
     &                      - coef * angular(2) * ml_o(inod)
        aiccg33(in*9-1) = aiccg33(in*9-1)                         &
     &                      + coef * angular(1) * ml_o(inod)
!        aiccg33(in*9  ) = aiccg33(in*9  )
      end do
!$omp end parallel do
!
      end subroutine cal_lumped_coriolis_matrix
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_consist_coriolis_matrix                            &
     &         (np_smp, numele, nnod_4_e1, nnod_4_e2,                   &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, idx_4_mat, k2,           &
     &          coef_cor, angular, dt, coef_imp, sk_v,                  &
     &          num_mat, aiccg33)
!
      integer(kind = kint), intent(in) :: np_smp, numele
      integer(kind = kint), intent(in) :: nnod_4_e1, nnod_4_e2
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &                  :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: iconn_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: idx_4_mat(num_sort_smp,nnod_4_e2)
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: angular(3)
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: num_mat
      real(kind = kreal), intent(inout) :: aiccg33(-8:num_mat)
!
      integer (kind = kint) :: iproc, iele, inum
      integer (kind = kint) :: mat_num
      integer (kind = kint) :: in, inn, iconn
      integer (kind = kint) :: istart, iend
      real(kind = kreal) :: coef
!
!
      coef = coef_imp * dt * coef_cor
!$omp parallel do private(inum,iele,iconn,inn,in,istart,iend,mat_num)
      do iproc = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(iproc-1)
          istart = nod_stack_smp(inn-1)+1
          iend = nod_stack_smp(inn)
!cdir nodep
          do in = istart, iend
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            mat_num = idx_4_mat(in,k2)
!
!            aiccg33(mat_num*9-8) = aiccg33(mat_num*9-8)
            aiccg33(mat_num*9-7) = aiccg33(mat_num*9-7)           &
     &           - sk_v(iele,1,iconn) * angular(3) * coef
            aiccg33(mat_num*9-6) = aiccg33(mat_num*9-6)           &
     &           + sk_v(iele,1,iconn) * angular(2) * coef
!
            aiccg33(mat_num*9-5) = aiccg33(mat_num*9-5)           &
     &           + sk_v(iele,1,iconn) * angular(3) * coef
!            aiccg33(mat_num*9-4) = aiccg33(mat_num*9-4)
            aiccg33(mat_num*9-3) = aiccg33(mat_num*9-3)           &
     &           - sk_v(iele,1,iconn) * angular(1) * coef
!
            aiccg33(mat_num*9-2) = aiccg33(mat_num*9-2)           &
     &           - sk_v(iele,1,iconn) * angular(2) * coef
            aiccg33(mat_num*9-1) = aiccg33(mat_num*9-1)           &
     &           + sk_v(iele,1,iconn) * angular(1) * coef
!            aiccg33(mat_num*9  ) = aiccg33(mat_num*9  )
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_consist_coriolis_matrix
!
!  ---------------------------------------------------------------------
!
      end module cal_coriolis_mat33
