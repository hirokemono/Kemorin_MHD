!
!      module sum_3d_ez_filter_phys_smp
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine sum_3d_ez_filter_scalar_smp(num_filter_grp,           &
!     &          id_filter_grp, ngrp_fil_smp, istack_fil_smp,           &
!     &          ntot_fil_smp, inod_fil_smp, istack_near_f_smp,         &
!     &          ntot_near_f_smp, inod_near_f_smp, filter_w_smp,        &
!     &          nnod_fil, x_vec_fil)
!      subroutine sum_3d_ez_filter_vector_smp(num_filter_grp,           &
!     &          id_filter_grp, ngrp_fil_smp, istack_fil_smp,           &
!     &          ntot_fil_smp, inod_fil_smp, istack_near_f_smp,         &
!     &          ntot_near_f_smp, inod_near_f_smp, filter_w_smp,        &
!     &          nnod_fil, x_vec_fil)
!      subroutine sum_3d_ez_filter_tensor_smp(num_filter_grp,           &
!     &          id_filter_grp, ngrp_fil_smp, istack_fil_smp,           &
!     &          ntot_fil_smp, inod_fil_smp, istack_near_f_smp,         &
!     &          ntot_near_f_smp, inod_near_f_smp, filter_w_smp,        &
!     &          nnod_fil, x_vec_fil)
!
      module sum_3d_ez_filter_phys_smp
!
      use m_precision
!
      use m_machine_parameter
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
      subroutine sum_3d_ez_filter_scalar_smp(num_filter_grp,            &
     &          id_filter_grp, ngrp_fil_smp, istack_fil_smp,            &
     &          ntot_fil_smp, inod_fil_smp, istack_near_f_smp,          &
     &          ntot_near_f_smp, inod_near_f_smp, filter_w_smp,         &
     &          nnod_fil, x_vec_fil)
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &              :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
      integer(kind = kint), intent(in) :: inod_fil_smp(ntot_fil_smp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_near_f_smp(0:ntot_fil_smp)
!
      integer(kind = kint), intent(in) :: ntot_near_f_smp
      integer(kind = kint), intent(in)                                  &
     &              :: inod_near_f_smp(ntot_near_f_smp)
      real(kind = kreal), intent(in) :: filter_w_smp(ntot_near_f_smp)
!
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(nnod_fil)
!
      integer(kind = kint) :: ist, ied, inum, inod, ip, jgrp
      integer(kind = kint) :: jst, jed, jnum, jnod, igrp, i
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
!
!$omp parallel do private(inod,jst,jed,jnum,jnod,jgrp,ip)
        do ip = 1, np_smp
          jgrp = (igrp-1)*np_smp + ip
          ist = istack_fil_smp(jgrp-1) + 1
          ied = istack_fil_smp(jgrp)
          do inum = ist, ied
            inod = inod_fil_smp(inum)
            jst = istack_near_f_smp(inum-1) + 1
            jed = istack_near_f_smp(inum)
            x_vec(inod) = 0.0d0
            do jnum = jst, jed
              jnod = inod_near_f_smp(jnum)
              x_vec(inod) = x_vec(inod)                                 &
     &                     + filter_w_smp(jnum) * x_vec_fil(jnod)
            end do
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine sum_3d_ez_filter_scalar_smp
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_ez_filter_vector_smp(num_filter_grp,            &
     &          id_filter_grp, ngrp_fil_smp, istack_fil_smp,            &
     &          ntot_fil_smp, inod_fil_smp, istack_near_f_smp,          &
     &          ntot_near_f_smp, inod_near_f_smp, filter_w_smp,         &
     &          nnod_fil, x_vec_fil)
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &              :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
      integer(kind = kint), intent(in) :: inod_fil_smp(ntot_fil_smp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_near_f_smp(0:ntot_fil_smp)
!
      integer(kind = kint), intent(in) :: ntot_near_f_smp
      integer(kind = kint), intent(in)                                  &
     &              :: inod_near_f_smp(ntot_near_f_smp)
      real(kind = kreal), intent(in) :: filter_w_smp(ntot_near_f_smp)
!
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(3*nnod_fil)
!
      integer(kind = kint) :: ist, ied, inum, inod, ip, jgrp
      integer(kind = kint) :: jst, jed, jnum, jnod, igrp, i
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
!
!$omp parallel do private(inod,jst,jed,jnum,jnod,jgrp,ip)
        do ip = 1, np_smp
          jgrp = (igrp-1)*np_smp + ip
          ist = istack_fil_smp(jgrp-1) + 1
          ied = istack_fil_smp(jgrp)
          do inum = ist, ied
            inod = inod_fil_smp(inum)
            jst = istack_near_f_smp(inum-1) + 1
            jed = istack_near_f_smp(inum)
            x_vec(3*inod-2) = 0.0d0
            x_vec(3*inod-1) = 0.0d0
            x_vec(3*inod  ) = 0.0d0
            do jnum = jst, jed
              jnod = inod_near_f_smp(jnum)
              x_vec(3*inod-2) = x_vec(3*inod-2)                         &
     &                          + filter_w_smp(jnum)                    &
     &                          * x_vec_fil(3*jnod-2)
              x_vec(3*inod-1) = x_vec(3*inod-1)                         &
     &                          + filter_w_smp(jnum)                    &
     &                          * x_vec_fil(3*jnod-1)
              x_vec(3*inod  ) = x_vec(3*inod  )                         &
     &                          + filter_w_smp(jnum)                    &
     &                          * x_vec_fil(3*jnod  )
            end do
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine sum_3d_ez_filter_vector_smp
!
!  ---------------------------------------------------------------------
!
      subroutine sum_3d_ez_filter_tensor_smp(num_filter_grp,            &
     &          id_filter_grp, ngrp_fil_smp, istack_fil_smp,            &
     &          ntot_fil_smp, inod_fil_smp, istack_near_f_smp,          &
     &          ntot_near_f_smp, inod_near_f_smp, filter_w_smp,         &
     &          nnod_fil, x_vec_fil)
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &              :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
      integer(kind = kint), intent(in) :: inod_fil_smp(ntot_fil_smp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_near_f_smp(0:ntot_fil_smp)
!
      integer(kind = kint), intent(in) :: ntot_near_f_smp
      integer(kind = kint), intent(in)                                  &
     &              :: inod_near_f_smp(ntot_near_f_smp)
      real(kind = kreal), intent(in) :: filter_w_smp(ntot_near_f_smp)
!
      integer(kind = kint), intent(in) :: nnod_fil
      real(kind = kreal), intent(in) :: x_vec_fil(6*nnod_fil)
!
      integer(kind = kint) :: ist, ied, inum, inod, ip, jgrp
      integer(kind = kint) :: jst, jed, jnum, jnod, igrp, i
!
!
      do i = 1, num_filter_grp
        igrp = id_filter_grp(i)
!
!$omp parallel do private(inod,jst,jed,jnum,jnod,jgrp,ip)
        do ip = 1, np_smp
          jgrp = (igrp-1)*np_smp + ip
          ist = istack_fil_smp(jgrp-1) + 1
          ied = istack_fil_smp(jgrp)
          do inum = ist, ied
            inod = inod_fil_smp(inum)
            jst = istack_near_f_smp(inum-1) + 1
            jed = istack_near_f_smp(inum)
            x_vec(6*inod-5) = 0.0d0
            x_vec(6*inod-4) = 0.0d0
            x_vec(6*inod-3) = 0.0d0
            x_vec(6*inod-2) = 0.0d0
            x_vec(6*inod-1) = 0.0d0
            x_vec(6*inod  ) = 0.0d0
            do jnum = jst, jed
              jnod = inod_near_f_smp(jnum)
              x_vec(6*inod-5) = x_vec(6*inod-5)                         &
     &                         + filter_w_smp(jnum)                     &
     &                          * x_vec_fil(6*jnod-5)
              x_vec(6*inod-4) = x_vec(6*inod-4)                         &
     &                         + filter_w_smp(jnum)                     &
     &                          * x_vec_fil(6*jnod-4)
              x_vec(6*inod-3) = x_vec(6*inod-3)                         &
     &                         + filter_w_smp(jnum)                     &
     &                          * x_vec_fil(6*jnod-3)
              x_vec(6*inod-2) = x_vec(6*inod-2)                         &
     &                         + filter_w_smp(jnum)                     &
     &                          * x_vec_fil(6*jnod-2)
              x_vec(6*inod-1) = x_vec(6*inod-1)                         &
     &                         + filter_w_smp(jnum)                     &
     &                          * x_vec_fil(6*jnod-1)
              x_vec(6*inod  ) = x_vec(6*inod  )                         &
     &                         + filter_w_smp(jnum)                     &
     &                          * x_vec_fil(6*jnod  )
            end do
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine sum_3d_ez_filter_tensor_smp
!
!  ---------------------------------------------------------------------
!
      end module sum_3d_ez_filter_phys_smp
