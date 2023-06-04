!>@file   cal_matrix_vector_smp.f90
!!        module cal_matrix_vector_smp
!!
!!@date  Programmed by H.Matsui on July, 2009
!!
!>@brief Make products of constant matrix with vector at each node
!!       need $omp parallel to use these routines 
!!
!!@verbatim
!!      subroutine cal_matvec_33_on_node(nnod, mat, vec, prod)
!!      subroutine cal_matvec_44_on_node(nnod, mat, vec, prod)
!!      subroutine cal_mat44_vec3_on_node(nnod, mat, vec, prod)
!!      subroutine cal_mat44_vec3_for_viz(nnod, mat, vec, prod)
!!
!!     definition of matrix
!!            / mat(1,1)  mat(1,2)  mat(1,3)  \
!!       A =  | mat(2,1)  mat(2,2)  mat(2,3)  |
!!            \ mat(3,1)  mat(3,2)  mat(3,3)  /
!!@endverbatim
!!
!!@n @param mat(3 or 4,3 or 4)      input matrix
!!@n @param vec(nnod,3 or 4)      nodal field
!!@n @param prod(nnod,3 or 4)     product
!
      module cal_matrix_vector_smp
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
      subroutine cal_matvec_33_on_node(nnod, mat, vec, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: mat(3,3)
      real (kind=kreal), intent(in) :: vec(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,3)
!
      integer (kind=kint) :: inod
!
!
!cdir nodep
!$omp parallel do private(inod)
        do inod = 1, nnod
          prod(inod,1) =  mat(1,1)*vec(inod,1)                          &
     &                  + mat(1,2)*vec(inod,2)                          &
     &                  + mat(1,3)*vec(inod,3)
          prod(inod,2) =  mat(2,1)*vec(inod,1)                          &
     &                  + mat(2,2)*vec(inod,2)                          &
     &                  + mat(2,3)*vec(inod,3)
          prod(inod,3) =  mat(3,1)*vec(inod,1)                          &
     &                  + mat(3,2)*vec(inod,2)                          &
     &                  + mat(3,3)*vec(inod,3)
        end do
!$omp end parallel do
!
      end subroutine cal_matvec_33_on_node
!
! ----------------------------------------------------------------------
!
      subroutine cal_matvec_44_on_node(nnod, mat, vec, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: mat(4,4)
      real (kind=kreal), intent(in) :: vec(nnod,4)
!
      real (kind=kreal), intent(inout) :: prod(nnod,4)
!
      integer (kind=kint) :: inod
!
!
!cdir nodep
!$omp parallel do private(inod)
        do inod = 1, nnod
          prod(inod,1) =  mat(1,1)*vec(inod,1)                          &
     &                  + mat(1,2)*vec(inod,2)                          &
     &                  + mat(1,3)*vec(inod,3)                          &
     &                  + mat(1,4)*vec(inod,4)
          prod(inod,2) =  mat(2,1)*vec(inod,1)                          &
     &                  + mat(2,2)*vec(inod,2)                          &
     &                  + mat(2,3)*vec(inod,3)                          &
     &                  + mat(2,4)*vec(inod,4)
          prod(inod,3) =  mat(3,1)*vec(inod,1)                          &
     &                  + mat(3,2)*vec(inod,2)                          &
     &                  + mat(3,3)*vec(inod,3)                          &
     &                  + mat(3,4)*vec(inod,4)
          prod(inod,4) =  mat(4,1)*vec(inod,1)                          &
     &                  + mat(4,2)*vec(inod,2)                          &
     &                  + mat(4,3)*vec(inod,3)                          &
     &                  + mat(4,4)*vec(inod,4)
        end do
!$omp end parallel do
!
      end subroutine cal_matvec_44_on_node
!
! ----------------------------------------------------------------------
!
      subroutine cal_mat44_vec3_on_node(nnod, mat, vec, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: mat(4,4)
      real (kind=kreal), intent(in) :: vec(nnod,3)
!
      real (kind=kreal), intent(inout) :: prod(nnod,4)
!
      integer (kind=kint) :: inod
!
!
!cdir nodep
!$omp parallel do private(inod)
        do inod = 1, nnod
          prod(inod,1) =  mat(1,1)*vec(inod,1)                          &
     &                  + mat(1,2)*vec(inod,2)                          &
     &                  + mat(1,3)*vec(inod,3)                          &
     &                  + mat(1,4)
          prod(inod,2) =  mat(2,1)*vec(inod,1)                          &
     &                  + mat(2,2)*vec(inod,2)                          &
     &                  + mat(2,3)*vec(inod,3)                          &
     &                  + mat(2,4)
          prod(inod,3) =  mat(3,1)*vec(inod,1)                          &
     &                  + mat(3,2)*vec(inod,2)                          &
     &                  + mat(3,3)*vec(inod,3)                          &
     &                  + mat(3,4)
          prod(inod,4) =  mat(4,1)*vec(inod,1)                          &
     &                  + mat(4,2)*vec(inod,2)                          &
     &                  + mat(4,3)*vec(inod,3)                          &
     &                  + mat(4,4)
      end do
!$omp end parallel do
!
      end subroutine cal_mat44_vec3_on_node
!
! ----------------------------------------------------------------------
!
      subroutine cal_mat44_vec3_for_viz(nnod, mat, vec, prod)
!
      integer (kind=kint), intent(in) :: nnod
      real (kind=kreal), intent(in) :: mat(4,4)
      real (kind=kreal), intent(in) :: vec(3,nnod)
!
      real (kind=kreal), intent(inout) :: prod(3,nnod)
!
      integer (kind=kint) :: inod
!
!
!cdir nodep
!$omp parallel do private(inod)
        do inod = 1, nnod
          prod(1,inod) =  mat(1,1)*vec(1,inod)                          &
     &                  + mat(1,2)*vec(2,inod)                          &
     &                  + mat(1,3)*vec(3,inod)                          &
     &                  + mat(1,4)
          prod(2,inod) =  mat(2,1)*vec(1,inod)                          &
     &                  + mat(2,2)*vec(2,inod)                          &
     &                  + mat(2,3)*vec(3,inod)                          &
     &                  + mat(2,4)
          prod(3,inod) =  mat(3,1)*vec(1,inod)                          &
     &                  + mat(3,2)*vec(2,inod)                          &
     &                  + mat(3,3)*vec(3,inod)                          &
     &                  + mat(3,4)
        end do
!$omp end parallel do
!
      end subroutine cal_mat44_vec3_for_viz
!
! ----------------------------------------------------------------------
!
      end module cal_matrix_vector_smp
