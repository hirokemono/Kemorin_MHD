!
!      module set_nodal_boundary
!
!      Written by H. Matsui on july, 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine set_nod_bc_from_ctl(num_phys_bc, ii, i, ibc_id,       &
!     &          ibc, ibc2, bc_id_apt, bc_magnitude )
!      subroutine set_nod_bc_from_data(num_phys_bc, ii, i, ibc_id,      &
!     &       ibc, ibc2, bc_id_apt, field_name )
!      subroutine set_fixed_bc_4_par_temp
!      subroutine set_potential_4_fixed_press
!      subroutine set_potential_4_sgs_press
!
      module set_nodal_boundary
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_bc_from_ctl(num_phys_bc, ii, i, ibc_id,        &
     &          ibc, ibc2, bc_id_apt, bc_magnitude )
!
      use m_geometry_parameter
      use m_node_group
!
      integer(kind = kint), intent(in) :: i
      integer(kind = kint), intent(in) :: num_phys_bc
      real ( kind = kreal), intent(in) :: bc_magnitude
!
      integer(kind = kint), intent(inout) :: ii
      integer(kind = kint), intent(inout) :: ibc_id(num_phys_bc)
      integer(kind = kint), intent(inout) :: ibc(numnod)
      integer(kind = kint), intent(inout) :: ibc2(numnod)
      real ( kind = kreal), intent(inout) :: bc_id_apt(num_phys_bc)
!
      integer(kind = kint) :: k
!
!
      do k=1, nod_grp1%istack_grp(i)-nod_grp1%istack_grp(i-1)
        ii=ii+1
!
        ibc_id(ii)=bc_item(k+nod_grp1%istack_grp(i-1))
        bc_id_apt(ii)=bc_magnitude
!
      end do
!
      if ( bc_magnitude .ne. 0.0d0 ) then
        do k=1, nod_grp1%istack_grp(i)-nod_grp1%istack_grp(i-1)
         ibc(    bc_item(k+nod_grp1%istack_grp(i-1)) ) = 1
        end do
      end if
!
      do k=1, nod_grp1%istack_grp(i)-nod_grp1%istack_grp(i-1)
        ibc2(    bc_item(k+nod_grp1%istack_grp(i-1)) ) = 1
      end do
!
      end subroutine set_nod_bc_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_bc_from_data(num_phys_bc, ii, i, ibc_id,       &
     &       ibc, ibc2, bc_id_apt, field_name )
!
      use m_geometry_parameter
      use m_node_group
      use m_boundary_field_IO
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i
      integer(kind = kint), intent(in) :: num_phys_bc
!
      integer(kind = kint), intent(inout) :: ii
      integer(kind = kint), intent(inout) :: ibc_id(num_phys_bc)
      integer(kind = kint), intent(inout) :: ibc(numnod), ibc2(numnod)
      real ( kind = kreal), intent(inout) :: bc_id_apt(num_phys_bc)
!
      integer(kind = kint) :: k, ja, ia
!
!
      do ia = 1, num_bc_group_IO
        if(bc_group_type_IO(ia) .eq. flag_nod_grp) then
          if ( bc_data_group_IO(ia) .eq. nod_grp1%grp_name(i)           &
     &       .and. bc_field_type_IO(ia) .eq. field_name ) then
!
            do k=1, nod_grp1%istack_grp(i)-nod_grp1%istack_grp(i-1)
              ja = istack_bc_data_IO(ia-1) + k
              ii=ii+1
!
              ibc_id(ii)=bc_item(k+nod_grp1%istack_grp(i-1))
              bc_id_apt(ii)=boundary_field_IO(ja)
!
              ibc(    bc_item(k+nod_grp1%istack_grp(i-1)) ) = 1
              ibc2(    bc_item(k+nod_grp1%istack_grp(i-1)) ) = 1
            end do
!
          end if
        end if
      end do
!
      end subroutine set_nod_bc_from_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_bc_4_par_temp
!
      use m_bc_data_ene
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint) :: inum, inod
!
      do inum=1, num_bc_e_nod
        inod = ibc_e_id(inum)
        bc_e_id_apt(inum) = bc_e_id_apt(inum)                           &
     &                     - d_nod(inod,iphys%i_ref_t)
      end do
!
      end subroutine set_fixed_bc_4_par_temp
!
!  ---------------------------------------------------------------------
!
      subroutine set_potential_4_fixed_press
!
      use m_bc_data_press
      use m_t_int_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
      integer (kind = kint) :: inum, inod
!
      do inum = 1, num_bc_p_nod
        inod = ibc_p_id(inum)
        bc_p_id_apt(inum) =   -dt * bc_p_id_apt(inum) * coef_press
        d_nod(inod,iphys%i_p_phi) = -dt * coef_press                    &
     &                             * d_nod(inod,iphys%i_press)
      end do
!
      end subroutine set_potential_4_fixed_press
!
!  ---------------------------------------------------------------------
!
      subroutine set_potential_4_sgs_press
!
      use m_bc_press_sgs
      use m_t_int_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
       integer (kind = kint) :: inum, inod
!
      do inum = 1, num_bc_ps_nod
        inod = ibc_ps_id(inum)
        bc_ps_id_apt(inum) =   -dt * bc_ps_id_apt(inum) * coef_press
        d_nod(inod,iphys%i_p_phi) =  -dt * coef_press                   &
     &                             * d_nod(inod,iphys%i_press)
      end do
!
      end subroutine set_potential_4_sgs_press
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_boundary
