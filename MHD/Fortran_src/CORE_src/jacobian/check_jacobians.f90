!
!     module check_jacobians
!
!     Written by H. Matsui on Aug., 2005
!
!      subroutine check_jacobians_trilinear(my_rank)
!      subroutine check_jacobians_triquad(my_rank)
!
      module check_jacobians
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_jacobians
!
      implicit  none
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine check_jacobians_trilinear(my_rank, ele, jac_3d)
!
      integer(kind = kint), intent(in) :: my_rank
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d
!
      integer(kind = kint) :: ii, k1, iele, nd
!
!
      write(50+my_rank,*) 'integration point, an'
      do ii = 1, jac_3d%ntot_int
          write(50+my_rank,'(i16,1p20e25.14)') ii,                    &
     &         (jac_3d%an(k1,ii),k1 = 1, num_t_linear)
      end do
!
!
      do ii = 1, jac_3d%ntot_int
        do nd = 1, 3
          write(50+my_rank,*) 'iele, integration point, dnx', ii, nd
          do iele = 1, ele%numele
            write(50+my_rank,'(i16,1p20e25.14)') iele,                  &
     &           (jac_3d%dnx(iele,k1,ii,nd), k1 = 1, num_t_linear)
          end do
        end do
      end do
!
      write(50+my_rank,*) 'iele, jacobian'
      do iele = 1, ele%numele
          write(50+my_rank,'(i16,1p20e25.14)') iele,                    &
     &           (jac_3d%xjac(iele,ii), k1 = 1, jac_3d%ntot_int)
      end do
!
      end subroutine check_jacobians_trilinear
!
!   ---------------------------------------------------------------------
!
      subroutine check_jacobians_triquad(my_rank, ele, jac_3d)
!
      integer(kind = kint), intent(in) :: my_rank
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(inout) :: jac_3d
!
      integer(kind = kint) :: ii, k1, iele, nd
!
!
      write(50+my_rank,*) 'integration point, aw'
      do ii = 1, jac_3d%ntot_int
          write(50+my_rank,'(i16,1p20e25.14)') ii,                      &
     &         (jac_3d%an(k1,ii),k1 = 1, ele%nnod_4_ele)
      end do
!
!
      do ii = 1, jac_3d%ntot_int
        do nd = 1, 3
          write(50+my_rank,*) 'iele, integration point, dwx', ii, nd
          do iele = 1, ele%numele
            write(50+my_rank,'(i16,1p20e25.14)') iele,                  &
     &           (jac_3d%dnx(iele,k1,ii,nd), k1 = 1, ele%nnod_4_ele)
          end do
        end do
      end do
!
!      write(50+my_rank,*) 'iele, jacobian'
!      do iele = 1, ele%numele
!          write(50+my_rank,'(i16,1p20e25.14)') iele,                   &
!     &           (jac_3d%xjac(iele,ii), k1 = 1, jac_3d%ntot_int)
!      end do
!
      end subroutine check_jacobians_triquad
!
!   ---------------------------------------------------------------------
!
      end module check_jacobians
