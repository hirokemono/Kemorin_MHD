!set_radial_magne_sph.f90
!     module set_radial_magne_sph
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine set_r_magne_sph                                      &
!!     &         (node, nod_grp, magne_nod, l_f, i, j, nod_bc_b)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(boundary_condition_list), intent(in) :: magne_nod
!!        type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_b
!
      module set_radial_magne_sph
!
      use m_precision
      use t_schmidt_polynomial
!
      implicit none
!
      integer(kind = kint), parameter, private :: ltr_ini = 10
      type(legendre_polynomials), private :: leg_b
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_r_magne_sph                                        &
     &         (node, nod_grp, magne_nod, l_f, i, j, nod_bc_b)
!
      use t_geometry_data
      use t_group_data
      use t_nodal_bc_data
      use t_bc_data_list
      use spherical_harmonics
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(boundary_condition_list), intent(in) :: magne_nod
      integer(kind = kint), intent(in) :: i, j
!
      integer(kind = kint), intent(inout) :: l_f(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_b
!
      integer(kind = kint) :: k
!
      integer(kind = kint) :: inod, nd
      integer(kind = kint) :: jj,ll,mm
      real ( kind = kreal) :: bmag
!
!
      call alloc_schmidt_polynomial(ltr_ini, leg_b)
!
      jj = int( aint( magne_nod%bc_magnitude(j)) )
      call get_dgree_order_by_full_j(jj, ll, mm)
!
      do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
        inod = nod_grp%item_grp(k+nod_grp%istack_grp(i-1))
!
        do nd = 1, 3
          l_f(nd) = l_f(nd) + 1
          nod_bc_b%ibc_id(l_f(nd),nd) = inod
        end do
!
        call dschmidt(node%theta(inod), leg_b)
!
        if (mm.ge.0) then
          bmag = leg_b%p(mm,ll) * cos( node%phi(inod)*dble(mm) )
        else
          bmag = leg_b%p(mm,ll) * sin( node%phi(inod)*dble(mm) )
        end if
!
        do nd = 1, 3
          nod_bc_b%ibc(inod,nd) = 1
          nod_bc_b%ibc2(inod,nd) = 1
          nod_bc_b%bc_apt(l_f(nd),nd) = bmag * node%xx(inod,1)          &
     &                                   * node%a_r(inod)
        end do
      end do
!
      call dealloc_schmidt_polynomial(leg_b)
!
      end subroutine set_r_magne_sph
!
!-----------------------------------------------------------------------
!
      end module set_radial_magne_sph
