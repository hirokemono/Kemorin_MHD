!>@file  int_edge_vector.f90
!!       module int_edge_vector
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2006
!
!> @brief  Structure of 1D Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine s_int_edge_vector(num_int, jac_1d, edge)
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        type(edge_data), intent(inout) :: edge
!!@endverbatim
!
      module int_edge_vector
!
      use m_precision
      use m_machine_parameter
      use t_edge_data
      use t_jacobian_1d
!
      implicit none
!
      private :: int_edge_vect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_edge_vector(num_int, jac_1d, edge)
!
      integer(kind = kint), intent(in) :: num_int
      type(jacobians_1d), intent(in) :: jac_1d
      type(edge_data), intent(inout) :: edge
!
!
      call int_edge_vect(edge%numedge, edge%istack_edge_smp,            &
     &    jac_1d%ntot_int, num_int, jac_1d%xj_edge, jac_1d%xeg_edge,    &
     &    edge%edge_vect, edge%edge_length, edge%a_edge_length)
!
      end subroutine s_int_edge_vector
!
!-----------------------------------------------------------------------
!
      subroutine int_edge_vect(numedge, iedge_smp_stack,                &
     &          ntot_int_1d, num_int, xj_edge, xeg_edge,                &
     &          edge_vect, edge_length, a_edge_length)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ntot_int_1d
      integer(kind = kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: xj_edge(numedge,ntot_int_1d)
      real(kind = kreal), intent(in) :: xeg_edge(numedge,ntot_int_1d,3)
!
      real(kind = kreal), intent(inout) :: edge_vect(numedge,3)
      real(kind = kreal), intent(inout) :: edge_length(numedge)
      real(kind = kreal), intent(inout) :: a_edge_length(numedge)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iedge, ix, ii
!
!
      edge_vect =     0.0d0
      edge_length =   0.0d0
      a_edge_length = 0.0d0
!
!$omp parallel do private(ist,ied,ii,ix,iedge)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
!
        do ii = 1, num_int
          ix = int_start1(num_int) + ii
!
!cdir noloopchg
          do iedge = ist, ied
            edge_length(iedge) = edge_length(iedge)                     &
     &                          + xj_edge(iedge,ix) * owe(ix)
!
            edge_vect(iedge,1) = edge_vect(iedge,1)                     &
     &                         + xeg_edge(iedge,ix,1) * owe(ix)
            edge_vect(iedge,2) = edge_vect(iedge,2)                     &
     &                         + xeg_edge(iedge,ix,2) * owe(ix)
            edge_vect(iedge,3) = edge_vect(iedge,3)                     &
     &                         + xeg_edge(iedge,ix,3) * owe(ix)
          end do
        end do
!
!cdir noloopchg
        do iedge = ist, ied
          if(edge_length(iedge) .eq. 0.0d0) then
            a_edge_length(iedge) = 1.0d60
          else
            a_edge_length(iedge) = 1.0d0 / edge_length(iedge)
          end if
        end do
!
!cdir noloopchg
        do iedge = ist, ied
          edge_vect(iedge,1) = edge_vect(iedge,1)*a_edge_length(iedge)
          edge_vect(iedge,2) = edge_vect(iedge,2)*a_edge_length(iedge)
          edge_vect(iedge,3) = edge_vect(iedge,3)*a_edge_length(iedge)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine int_edge_vect
!
! ----------------------------------------------------------------------
!
      end module int_edge_vector
