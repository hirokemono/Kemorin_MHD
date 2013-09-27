!
!      module sum_3d_ez_filter_phys
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on May, 2008
!
!      subroutine sum_3d_ez_filter_scalar_phys(num_filter_grp,          &
!     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,         &
!     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,     &
!     &          filter_w, nnod_fil, x_vec_fil, nnod, x_vec)
!      subroutine sum_3d_ez_filter_vector_phys(num_filter_grp,          &
!     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,         &
!     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,     &
!     &          filter_w, nnod_fil, x_vec_fil, nnod, x_vec)
!      subroutine sum_3d_ez_filter_tensor_phys(num_filter_grp,          &
!     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,         &
!     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,     &
!     &          filter_w, nnod_fil, x_vec_fil, nnod, x_vec)
!
      module sum_3d_ez_filter_phys
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_ez_filter_scalar_phys(num_filter_grp,           &
     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,          &
     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,      &
     &          filter_w, nnod_fil, x_vec_fil, nnod, x_vec)
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
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(nnod_fil)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: x_vec(nnod)
!
      integer(kind = kint) :: ist, ied, inum, inod, i, igrp
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
!$omp parallel do private(inod,jst,jed,jnum,jnod)
        do inum = ist, ied
          inod = inod_fil(inum)
          jst = istack_near_f(inum-1) + 1
          jed = istack_near_f(inum)
          x_vec(inod) = 0.0d0
          do jnum = jst, jed
            jnod = inod_near_f(jnum)
            x_vec(inod) = x_vec(inod)                                   &
     &         + filter_w(jnum) * x_vec_fil(jnod)
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine sum_3d_ez_filter_scalar_phys
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_ez_filter_vector_phys(num_filter_grp,           &
     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,          &
     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,      &
     &          filter_w, nnod_fil, x_vec_fil, nnod, x_vec)
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
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(3*nnod_fil)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: x_vec(3*nnod)
!
      integer(kind = kint) :: ist, ied, inum, inod, i, igrp
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
!$omp parallel do private(inod,jst,jed,jnum,jnod)
        do inum = ist, ied
          inod = inod_fil(inum)
          jst = istack_near_f(inum-1) + 1
          jed = istack_near_f(inum)
          x_vec(3*inod-2) = 0.0d0
          x_vec(3*inod-1) = 0.0d0
          x_vec(3*inod  ) = 0.0d0
          do jnum = jst, jed
            jnod = inod_near_f(jnum)
            x_vec(3*inod-2) = x_vec(3*inod-2)                           &
     &         + filter_w(jnum) * x_vec_fil(3*jnod-2)
            x_vec(3*inod-1) = x_vec(3*inod-1)                           &
     &         + filter_w(jnum) * x_vec_fil(3*jnod-1)
            x_vec(3*inod  ) = x_vec(3*inod  )                           &
     &         + filter_w(jnum) * x_vec_fil(3*jnod  )
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine sum_3d_ez_filter_vector_phys
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_ez_filter_tensor_phys(num_filter_grp,           &
     &          id_filter_grp, ngrp_fil, istack_fil, ntot_fil,          &
     &          inod_fil, istack_near_f, ntot_near_f, inod_near_f,      &
     &          filter_w, nnod_fil, x_vec_fil, nnod, x_vec)
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
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(6*nnod_fil)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: x_vec(6*nnod)
!
      integer(kind = kint) :: ist, ied, inum, inod, i, igrp
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
!$omp parallel do private(inod,jst,jed,jnum,jnod)
        do inum = ist, ied
          inod = inod_fil(inum)
          jst = istack_near_f(inum-1) + 1
          jed = istack_near_f(inum)
          x_vec(6*inod-5) = 0.0d0
          x_vec(6*inod-4) = 0.0d0
          x_vec(6*inod-3) = 0.0d0
          x_vec(6*inod-2) = 0.0d0
          x_vec(6*inod-1) = 0.0d0
          x_vec(6*inod  ) = 0.0d0
          do jnum = jst, jed
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
        end do
!$omp end parallel do
      end do
!
      end subroutine sum_3d_ez_filter_tensor_phys
!
!  ---------------------------------------------------------------------
!
      end module sum_3d_ez_filter_phys
