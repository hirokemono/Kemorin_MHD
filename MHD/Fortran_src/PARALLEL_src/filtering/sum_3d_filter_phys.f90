!
!      module sum_3d_filter_phys
!
!     Written by H. Matsui on Sep., 2007
!     Modified by H. Matsui on May, 2008
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine sum_3d_filter_scalar_phys(num_filter_grp,             &
!     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,         &
!     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,     &
!     &          filter_w, istack_sum_fil, max_sum_fil, ntot_sum_fil,   &
!     &          ist_sum_fil, ied_sum_fil, nnod_fil, x_vec_fil)
!      subroutine sum_3d_filter_vector_phys(num_filter_grp,             &
!     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,         &
!     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,     &
!     &          filter_w, istack_sum_fil, max_sum_fil, ntot_sum_fil,   &
!     &          ist_sum_fil, ied_sum_fil, nnod_fil, x_vec_fil)
!      subroutine sum_3d_filter_tensor_phys(num_filter_grp,             &
!     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,         &
!     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,     &
!     &          filter_w, istack_sum_fil, max_sum_fil, ntot_sum_fil,   &
!     &          ist_sum_fil, ied_sum_fil, nnod_fil, x_vec_fil)
!
      module sum_3d_filter_phys
!
      use m_precision
!
      use m_parallel_var_dof
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_filter_scalar_phys(num_filter_grp,              &
     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,          &
     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,      &
     &          filter_w, istack_sum_fil, max_sum_fil, ntot_sum_fil,    &
     &          ist_sum_fil, ied_sum_fil, nnod_fil, x_vec_fil)
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: inod_fil(ntot_fil)
      integer(kind = kint), intent(in) :: istack_near_f(0:ntot_fil)
      integer(kind = kint), intent(in) :: ntot_near_f
      integer(kind = kint), intent(in) :: inod_near_f(ntot_near_f)
      real(kind = kreal), intent(in) :: filter_w(ntot_near_f)
!
      integer(kind = kint), intent(in) :: istack_sum_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: max_sum_fil(ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_sum_fil
      integer(kind = kint), intent(in) :: ist_sum_fil(ntot_sum_fil)
      integer(kind = kint), intent(in) :: ied_sum_fil(ntot_sum_fil)
!
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(nnod_fil)
!
      integer(kind = kint) :: igrp, ist, ied, inum, inod, ii, i
      integer(kind = kint) :: jnum, jnod, knum, kst, ked
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
!
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
!$omp parallel do private(inod)
        do inum = ist, ied
          inod = inod_fil(inum)
          x_vec(inod  ) = 0.0d0
        end do
!$omp end parallel do
!
        ist = istack_sum_fil(igrp-1)
        do inum = 1, max_sum_fil(igrp)
          ii = ist + inum
          kst = ist_sum_fil(ii)
          ked = ied_sum_fil(ii)
!$omp parallel do private(inod,jnum,jnod,knum)
!cdir nodep noloopchg
          do knum = kst, ked
            inod = inod_fil(knum)
            jnum = istack_near_f(knum-1) + inum
            jnod = inod_near_f(jnum)
            x_vec(inod  ) = x_vec(inod  )                               &
     &          + filter_w(jnum) * x_vec_fil(jnod)
          end do
!$omp end parallel do
        end do
      end do
!
      end subroutine sum_3d_filter_scalar_phys
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_filter_vector_phys(num_filter_grp,              &
     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,          &
     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,      &
     &          filter_w, istack_sum_fil, max_sum_fil, ntot_sum_fil,    &
     &          ist_sum_fil, ied_sum_fil, nnod_fil, x_vec_fil)
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: inod_fil(ntot_fil)
      integer(kind = kint), intent(in) :: istack_near_f(0:ntot_fil)
      integer(kind = kint), intent(in) :: ntot_near_f
      integer(kind = kint), intent(in) :: inod_near_f(ntot_near_f)
      real(kind = kreal), intent(in) :: filter_w(ntot_near_f)
!
      integer(kind = kint), intent(in) :: istack_sum_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: max_sum_fil(ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_sum_fil
      integer(kind = kint), intent(in) :: ist_sum_fil(ntot_sum_fil)
      integer(kind = kint), intent(in) :: ied_sum_fil(ntot_sum_fil)
!
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(3*nnod_fil)
!
      integer(kind = kint) :: igrp, ist, ied, inum, inod, ii, i
      integer(kind = kint) :: jnum, jnod, knum, kst, ked
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
!
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
!$omp parallel do private(inod)
        do inum = ist, ied
          inod = inod_fil(inum)
          x_vec(3*inod-2) = 0.0d0
          x_vec(3*inod-1) = 0.0d0
          x_vec(3*inod  ) = 0.0d0
        end do
!$omp end parallel do
!
        ist = istack_sum_fil(igrp-1)
        do inum = 1, max_sum_fil(igrp)
          ii = ist + inum
          kst = ist_sum_fil(ii)
          ked = ied_sum_fil(ii)
!$omp parallel do private(inod,jnum,jnod,knum)
!cdir nodep noloopchg
          do knum = kst, ked
            inod = inod_fil(knum)
            jnum = istack_near_f(knum-1) + inum
            jnod = inod_near_f(jnum)
            x_vec(3*inod-2) = x_vec(3*inod-2)                           &
     &         + filter_w(jnum) * x_vec_fil(3*jnod-2)
            x_vec(3*inod-1) = x_vec(3*inod-1)                           &
     &         + filter_w(jnum) * x_vec_fil(3*jnod-1)
            x_vec(3*inod  ) = x_vec(3*inod  )                           &
     &         + filter_w(jnum) * x_vec_fil(3*jnod  )
          end do
!$omp end parallel do
        end do
      end do
!
      end subroutine sum_3d_filter_vector_phys
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_filter_tensor_phys(num_filter_grp,              &
     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,          &
     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,      &
     &          filter_w, istack_sum_fil, max_sum_fil, ntot_sum_fil,    &
     &          ist_sum_fil, ied_sum_fil, nnod_fil, x_vec_fil)
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: inod_fil(ntot_fil)
      integer(kind = kint), intent(in) :: istack_near_f(0:ntot_fil)
      integer(kind = kint), intent(in) :: ntot_near_f
      integer(kind = kint), intent(in) :: inod_near_f(ntot_near_f)
      real(kind = kreal), intent(in) :: filter_w(ntot_near_f)
!
      integer(kind = kint), intent(in) :: istack_sum_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: max_sum_fil(ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_sum_fil
      integer(kind = kint), intent(in) :: ist_sum_fil(ntot_sum_fil)
      integer(kind = kint), intent(in) :: ied_sum_fil(ntot_sum_fil)
!
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(6*nnod_fil)
!
      integer(kind = kint) :: igrp, ist, ied, inum, inod, ii, i
      integer(kind = kint) :: jnum, jnod, knum, kst, ked
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
!
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
!$omp parallel do private(inod)
        do inum = ist, ied
          inod = inod_fil(inum)
          x_vec(6*inod-5) = 0.0d0
          x_vec(6*inod-4) = 0.0d0
          x_vec(6*inod-3) = 0.0d0
          x_vec(6*inod-2) = 0.0d0
          x_vec(6*inod-1) = 0.0d0
          x_vec(6*inod  ) = 0.0d0
        end do
!$omp end parallel do
!
        ist = istack_sum_fil(igrp-1)
        do inum = 1, max_sum_fil(igrp)
          ii = ist + inum
          kst = ist_sum_fil(ii)
          ked = ied_sum_fil(ii)
!$omp parallel do private(inod,jnum,jnod,knum)
!cdir nodep noloopchg
          do knum = kst, ked
            inod = inod_fil(knum)
            jnum = istack_near_f(knum-1) + inum
            jnod = inod_near_f(jnum)
            x_vec(6*inod-5) = x_vec(6*inod-5)                           &
     &         + filter_w(jnum) * x_vec_fil(6*jnod-5)
            x_vec(6*inod-4) = x_vec(6*inod-4)                           &
     &         + filter_w(jnum) * x_vec_fil(6*jnod-4)
            x_vec(6*inod-3) = x_vec(6*inod-3)                           &
     &         + filter_w(jnum) * x_vec_fil(6*jnod-3)
            x_vec(6*inod-2) = x_vec(6*inod-2)                           &
     &         + filter_w(jnum) * x_vec_fil(6*jnod-2)
            x_vec(6*inod-1) = x_vec(6*inod-1)                           &
     &         + filter_w(jnum) * x_vec_fil(6*jnod-1)
            x_vec(6*inod  ) = x_vec(6*inod  )                           &
     &         + filter_w(jnum) * x_vec_fil(6*jnod  )
          end do
!$omp end parallel do
        end do
      end do
!
      end subroutine sum_3d_filter_tensor_phys
!
!  ---------------------------------------------------------------------
!
      end module sum_3d_filter_phys
