!overwrite_prod_const_smp.f90
!     module overwrite_prod_const_smp
!
!        programmed by H.Matsui on May., 2009
!
!      need $omp parallel to use routines
!
!      subroutine ovwrt_coef_prod_scalar_smp(np_smp, nnod,              &
!     &          inod_smp_stack, coef, prod)
!      subroutine ovwrt_coef_prod_vect_smp(np_smp, nnod,                &
!     &          inod_smp_stack, coef, prod)
!      subroutine ovwrt_coef_prod_tensor_smp(np_smp, nnod,              &
!     &          inod_smp_stack, coef, prod)
!
!      subroutine ovwrt_vect_prod_cvec_coef_smp(np_smp, nnod,           &
!     &          inod_smp_stack, coef, c_vec, prod)
!             prod(:,:) = coef * c_vec(:) \times prod(:,:)
!      subroutine ovwrt_vect_prod_cvec_smp(np_smp, nnod,                &
!     &          inod_smp_stack, c_vec, prod)
!             prod(:,:) = c_vec(:) \times prod(:,:)
!
      module overwrite_prod_const_smp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_coef_prod_scalar_smp(np_smp, nnod,               &
     &          inod_smp_stack, coef, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod) =  prod(inod)*coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_coef_prod_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_coef_prod_vect_smp(np_smp, nnod,                 &
     &          inod_smp_stack, coef, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) =  coef*prod(inod,1)
          prod(inod,2) =  coef*prod(inod,2)
          prod(inod,3) =  coef*prod(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_coef_prod_vect_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_coef_prod_tensor_smp(np_smp, nnod,               &
     &          inod_smp_stack, coef, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: prod(nnod,6)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          prod(inod,1) =  coef*prod(inod,1)
          prod(inod,2) =  coef*prod(inod,2)
          prod(inod,3) =  coef*prod(inod,3)
          prod(inod,4) =  coef*prod(inod,4)
          prod(inod,5) =  coef*prod(inod,5)
          prod(inod,6) =  coef*prod(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_coef_prod_tensor_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ovwrt_vect_prod_cvec_coef_smp(np_smp, nnod,            &
     &          inod_smp_stack, coef, c_vec, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
      real (kind=kreal) :: v(3)
!
!
!$omp do private(inod,ist,ied,v)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          v(1) = prod(inod,1)
          v(2) = prod(inod,2)
          v(3) = prod(inod,3)
          prod(inod,1) = (c_vec(2)*v(3) - c_vec(3)*v(2)) * coef
          prod(inod,2) = (c_vec(3)*v(1) - c_vec(1)*v(3)) * coef
          prod(inod,3) = (c_vec(1)*v(2) - c_vec(2)*v(1)) * coef
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vect_prod_cvec_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine ovwrt_vect_prod_cvec_smp(np_smp, nnod,                 &
     &          inod_smp_stack, c_vec, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: c_vec(3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
      real (kind=kreal) :: v(3)
!
!
!$omp do private(inod,ist,ied,v)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          v(1) = prod(inod,1)
          v(2) = prod(inod,2)
          v(3) = prod(inod,3)
          prod(inod,1) = c_vec(2)*v(3) - c_vec(3)*v(2)
          prod(inod,2) = c_vec(3)*v(1) - c_vec(1)*v(3)
          prod(inod,3) = c_vec(1)*v(2) - c_vec(2)*v(1)
        end do
      end do
!$omp end do nowait
!
      end subroutine ovwrt_vect_prod_cvec_smp
!
! ----------------------------------------------------------------------
!
      end module overwrite_prod_const_smp
