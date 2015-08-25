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
      use m_geometry_data
!
      implicit  none
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine check_jacobians_trilinear(my_rank)
!
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: ii, k1, iele, nd
!
!
      write(50+my_rank,*) 'integration point, an'
      do ii = 1, jac1_3d_l%ntot_int
          write(50+my_rank,'(i16,1p20e25.14)') ii,                    &
     &         (jac1_3d_l%an(k1,ii),k1 = 1, num_t_linear)
      end do
!
!
      do ii = 1, jac1_3d_l%ntot_int
       do nd = 1, 3
        write(50+my_rank,*) 'iele, integration point, dnx', ii, nd
        do iele = 1, ele1%numele
          write(50+my_rank,'(i16,1p20e25.14)') iele,                    &
     &           (jac1_3d_l%dnx(iele,k1,ii,nd), k1 = 1, num_t_linear)
        end do
       end do
      end do
!
      write(50+my_rank,*) 'iele, jacobian'
      do iele = 1, ele1%numele
          write(50+my_rank,'(i16,1p20e25.14)') iele,                    &
     &           (jac1_3d_l%xjac(iele,ii), k1 = 1, jac1_3d_l%ntot_int)
      end do
!
      end subroutine check_jacobians_trilinear
!
!   ---------------------------------------------------------------------
!
      subroutine check_jacobians_triquad(my_rank)
!
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: ii, k1, iele, nd
!
!
      write(50+my_rank,*) 'integration point, aw'
      do ii = 1, jac1_3d_q%ntot_int
          write(50+my_rank,'(i16,1p20e25.14)') ii,                      &
     &         (jac1_3d_q%an(k1,ii),k1 = 1, num_t_linear)
      end do
!
!
      do ii = 1, jac1_3d_q%ntot_int
       do nd = 1, 3
        write(50+my_rank,*) 'iele, integration point, dwx', ii, nd
        do iele = 1, ele1%numele
          write(50+my_rank,'(i16,1p20e25.14)') iele,                    &
     &           (dwx(iele,k1,ii,nd), k1 = 1, ele1%nnod_4_ele)
        end do
       end do
      end do
!
!      write(50+my_rank,*) 'iele, jacobian'
!      do iele = 1, ele1%numele
!          write(50+my_rank,'(i16,1p20e25.14)') iele,                   &
!     &           (jac1_3d_q%xjac(iele,ii), k1 = 1, jac1_3d_q%ntot_int)
!      end do
!
      end subroutine check_jacobians_triquad
!
!   ---------------------------------------------------------------------
!
      end module check_jacobians
