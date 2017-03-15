!
!      module set_nodal_boundary
!
!      Written by H. Matsui on july, 2005
!      Modified by H. Matsui on Jan., 2009
!
!!      subroutine set_nod_bc_from_ctl(nod_grp, numnod, num_phys_bc,    &
!!     &          ii, i, ibc_id, ibc, ibc2, bc_id_apt, bc_magnitude )
!!      subroutine set_nod_bc_from_data                                 &
!!     &         (IO_bc, nod_grp, numnod, num_phys_bc,                  &
!!     &          ii, i, ibc_id, ibc, ibc2, bc_id_apt, field_name)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(group_data), intent(in) :: nod_grp
!!      subroutine set_fixed_bc_per_scalar                              &
!!     &         (numnod, ncomp_nod, i_ref_t, d_nod, nod_bc_t)
!!      subroutine set_potential_4_fixed_press(dt, coef_press, nod_bc_p)
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
      subroutine set_nod_bc_from_ctl(nod_grp, numnod, num_phys_bc,      &
     &          ii, i, ibc_id, ibc, ibc2, bc_id_apt, bc_magnitude )
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(in) :: i
      integer(kind = kint), intent(in) :: num_phys_bc
      real ( kind = kreal), intent(in) :: bc_magnitude
!
      integer(kind = kint), intent(inout) :: ii
      integer(kind = kint), intent(inout) :: ibc_id(num_phys_bc)
      integer(kind = kint), intent(inout) :: ibc(numnod)
      integer(kind = kint), intent(inout) :: ibc2(numnod)
      real(kind = kreal), intent(inout) :: bc_id_apt(num_phys_bc)
!
      integer(kind = kint) :: k
!
!
      do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
        ii=ii+1
!
        ibc_id(ii)=nod_grp%item_grp(k+nod_grp%istack_grp(i-1))
        bc_id_apt(ii)=bc_magnitude
!
      end do
!
      if ( bc_magnitude .ne. 0.0d0 ) then
        do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
         ibc(nod_grp%item_grp(k+nod_grp%istack_grp(i-1)) ) = 1
        end do
      end if
!
      do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
        ibc2(nod_grp%item_grp(k+nod_grp%istack_grp(i-1)) ) = 1
      end do
!
      end subroutine set_nod_bc_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_bc_from_data                                   &
     &         (IO_bc, nod_grp, numnod, num_phys_bc,                    &
     &          ii, i, ibc_id, ibc, ibc2, bc_id_apt, field_name)
!
      use t_group_data
      use t_boundary_field_IO
!
      integer(kind = kint), intent(in) :: numnod
      type(IO_boundary), intent(in) :: IO_bc
      type(group_data), intent(in) :: nod_grp
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
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_nod_grp) then
          if ( IO_bc%group_name(ia) .eq. nod_grp%grp_name(i)            &
     &       .and. IO_bc%field_type(ia) .eq. field_name ) then
!
            do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
              ja = IO_bc%istack_data(ia-1) + k
              ii=ii+1
!
              ibc_id(ii)=nod_grp%item_grp(k+nod_grp%istack_grp(i-1))
              bc_id_apt(ii)=IO_bc%d_field(ja)
!
              ibc( nod_grp%item_grp(k+nod_grp%istack_grp(i-1)) ) = 1
              ibc2(nod_grp%item_grp(k+nod_grp%istack_grp(i-1)) ) = 1
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
      subroutine set_fixed_bc_per_scalar                                &
     &         (numnod, ncomp_nod, i_ref_t, d_nod, nod_bc_t)
!
      use t_nodal_bc_data
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_ref_t
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_t
!
      integer (kind = kint) :: inum, inod
!
      do inum = 1, nod_bc_t%num_bc_nod
        inod = nod_bc_t%ibc_id(inum)
        nod_bc_t%bc_apt(inum) = nod_bc_t%bc_apt(inum)                   &
     &                          - d_nod(inod,i_ref_t)
      end do
!
      end subroutine set_fixed_bc_per_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine set_potential_4_fixed_press(dt, coef_press, nod_bc_p)
!
      use t_nodal_bc_data
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_press
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_p
!
       integer (kind = kint) :: inum, inod
!
      do inum = 1, nod_bc_p%num_bc_nod
        inod = nod_bc_p%ibc_id(inum)
        nod_bc_p%bc_apt(inum)                                           &
     &       = -dt * coef_press * nod_bc_p%bc_apt(inum)
      end do
!
      end subroutine set_potential_4_fixed_press
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_boundary
