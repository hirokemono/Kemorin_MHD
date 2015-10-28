!set_radial_magne_sph.f90
!     module set_radial_magne_sph
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine set_r_magne_sph(node, nod_grp, l_f, i, j,            &
!!     &          ncomp_nod, i_magne, d_nod)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!
      module set_radial_magne_sph
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
      subroutine set_r_magne_sph(node, nod_grp, l_f, i, j,              &
     &          ncomp_nod, i_magne, d_nod)
!
      use t_geometry_data
      use t_group_data
      use m_bc_data_list
      use m_bc_data_magne
      use m_schmidt_polynomial
      use spherical_harmonics
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint), intent(in) :: i, j
      integer (kind = kint), intent(in) :: ncomp_nod, i_magne
!
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
      integer(kind = kint), intent(inout) :: l_f(3)
!
      integer(kind = kint) :: k
!
      integer(kind = kint) :: inod, nd, i_comp
      integer(kind = kint) :: jj,ll,mm
      real ( kind = kreal) :: bmag
!
!
      call allocate_schmidt_polynomial
!
      jj = int( aint( magne_nod%bc_magnitude(j)) )
      call get_dgree_order_by_full_j(jj, ll, mm)
!
      do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
        inod = nod_grp%item_grp(k+nod_grp%istack_grp(i-1))
!
        do nd = 1, 3
          l_f(nd) = l_f(nd) + 1
          nod_bc1_b%ibc_id(l_f(nd),nd) = inod
        end do
!
        call dschmidt(node%theta(inod))
!
        if (mm.ge.0) then
          bmag = p(mm,ll) * cos( node%phi(inod)*dble(mm) )
        else
          bmag = p(mm,ll) * sin( node%phi(inod)*dble(mm) )
        end if
!
        do nd = 1, 3
          i_comp = i_magne + nd - 1
          nod_bc1_b%ibc(inod,nd) = 1
          nod_bc1_b%ibc2(inod,nd) = 1
          bc_b_id_apt(l_f(nd),nd) = bmag * node%xx(inod,1)              &
     &                                   * node%a_r(inod)
          d_nod(inod,i_comp) = bc_b_id_apt(l_f(nd),nd)
        end do
      end do
!
      call deallocate_schmidt_polynomial
!
      end subroutine set_r_magne_sph
!
!-----------------------------------------------------------------------
!
      end module set_radial_magne_sph
