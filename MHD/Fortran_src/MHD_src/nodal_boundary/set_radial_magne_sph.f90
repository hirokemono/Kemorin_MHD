!set_radial_magne_sph.f90
!     module set_radial_magne_sph
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine set_r_magne_sph(nod_grp, l_f, i, j)
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
      subroutine set_r_magne_sph(nod_grp, l_f, i, j)
!
      use t_group_data
      use m_bc_data_list
      use m_bc_data_magne
      use m_geometry_data
      use m_geometry_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_schmidt_polynomial
      use spherical_harmonics
!
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint), intent(in) :: i, j
      integer(kind = kint), intent(inout) :: l_f(3)
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
          ibc_b_id(l_f(nd),nd) = inod
        end do
!
        call dschmidt(colatitude(inod))
!
        if (mm.ge.0) then
          bmag = p(mm,ll) * cos( longitude(inod)*dble(mm) )
        else
          bmag = p(mm,ll) * sin( longitude(inod)*dble(mm) )
        end if
!
        do nd = 1, 3
          i_comp = iphys%i_magne + nd - 1
          ibc_magne(inod,nd) = 1
          ibc2_magne(inod,nd) = 1
          bc_b_id_apt(l_f(nd),nd) = bmag * xx(inod,1) *a_radius(inod)
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
