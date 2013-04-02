!
!     module correct_matrix_4_boundary
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui  on July 2006
!
!      subroutine correct_matrix11_4_boundary                           &
!     &          (numnod, itotal_u, itotal_l, mat_num, aiccg)
!      subroutine correct_matrix33_4_boundary                           &
!     &          (numnod, itotal_u, itotal_l, nd, mat_num, aiccg)
!
      module correct_matrix_4_boundary
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine correct_matrix11_4_boundary                            &
     &          (numnod, itotal_u, itotal_l, mat_num, aiccg)
!
      integer(kind = kint), intent(in) :: mat_num
      integer(kind = kint), intent(in) :: numnod, itotal_u, itotal_l
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: aiccg(0:numnod+itotal_l+itotal_u)
!
!
      if ( mat_num .le. numnod ) then
        aiccg(mat_num)= 1.0d0
      else
        aiccg(mat_num)= 0.0d0
      end if
!
      end subroutine correct_matrix11_4_boundary
!
!-----------------------------------------------------------------------
!
      subroutine correct_matrix33_4_boundary                            &
     &          (numnod, itotal_u, itotal_l, nd, mat_num, aiccg)
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: nd, mat_num
      integer (kind = kint), intent(in) :: itotal_l, itotal_u
!
      real (kind=kreal), intent(inout)                                  &
     &    :: aiccg(-8:9*(numnod+itotal_l+itotal_u))
!
!
      aiccg(mat_num*9+nd-9)  = 0.0d0 
      aiccg(mat_num*9+nd-6)  = 0.0d0 
      aiccg(mat_num*9+nd-3)  = 0.0d0
!
      if ( mat_num .le. numnod ) then 
        aiccg(mat_num*9+nd*4-12)  = 1.0d0
      end if
!
      end subroutine correct_matrix33_4_boundary
!
!-----------------------------------------------------------------------
!
      end module correct_matrix_4_boundary
