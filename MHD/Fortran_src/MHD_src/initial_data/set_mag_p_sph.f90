!
!      module set_mag_p_sph
!
!      Written by H. Matsui
!
!     subroutine set_mag_p_sph
!
      module set_mag_p_sph
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
      subroutine s_set_mag_p_sph(ii, i, j)
!
      use m_precision
!
      use m_node_group
      use m_bc_data_list
      use m_bc_data_magne_p
      use m_geometry_parameter
      use m_geometry_data
      use m_schmidt_polynomial
      use spherical_harmonics
!
      implicit none
!
      integer(kind = kint) :: i, j, k, ii, jj
!
      integer(kind = kint) :: inod
      integer(kind = kint) :: ll, mm
!
!
      call allocate_schmidt_polynomial
!
!
      jj = int(e_potential_nod%bc_magnitude(j))
      call get_dgree_order_by_full_j(jj, ll, mm)
!
      do k=1, nod_grp1%istack_grp(i)-nod_grp1%istack_grp(i-1)
       ii=ii+1
!
       inod = bc_item(k+nod_grp1%istack_grp(i-1))
       ibc_mag_p_id(ii)=inod
!
       call dschmidt(colatitude(inod))
!
       if (mm.ge.0) then
         bc_mag_p_id_apt(ii) = p(mm,ll) * cos(longitude(inod)*dble(mm))
       else
         bc_mag_p_id_apt(ii) = p(mm,ll) * sin(longitude(inod)*dble(mm))
       end if
!
        ibc_mag_p(    inod ) = 1
        ibc2_mag_p(   inod ) = 1
!
      end do
!
      call deallocate_schmidt_polynomial
!
      end subroutine s_set_mag_p_sph
!
!-----------------------------------------------------------------------
!
      end module set_mag_p_sph
