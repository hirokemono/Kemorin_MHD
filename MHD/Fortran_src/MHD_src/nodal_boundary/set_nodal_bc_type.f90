!
!      module set_nodal_bc_type
!
!      Written by H. Matsui on july, 2005
!      Modified by H. Matsui on Jan., 2009
!
!!      subroutine set_nod_bc_type_from_ctl(ii, i, nod_grp,             &
!!     &          scalar_bc, bc_magnitude )
!!        integer(kind = kint), intent(in) :: i
!!        real ( kind = kreal), intent(in) :: bc_magnitude
!!        type(group_data), intent(in) :: nod_grp
!!        integer(kind = kint), intent(inout) :: ii
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!!      subroutine set_vect_nod_bc_type_from_ctl(ii, i, nd, nod_grp,    &
!!     &          vect_bc, bc_magnitude )
!!        integer(kind = kint), intent(in) :: i, nd
!!        real ( kind = kreal), intent(in) :: bc_magnitude
!!        type(group_data), intent(in) :: nod_grp
!!        integer(kind = kint), intent(inout) :: ii
!!        type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!!      subroutine set_rot_nod_bc_type_from_ctl(ii, i, nd, nod_grp,     &
!!     &          rotation, bc_magnitude )
!!        integer(kind = kint), intent(in) :: i, nd
!!        real ( kind = kreal), intent(in) :: bc_magnitude
!!        type(group_data), intent(in) :: nod_grp
!!        integer(kind = kint), intent(inout) :: ii(3)
!!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotation
!!      subroutine set_nod_bc_type_from_data(ii, i, IO_bc, nod_grp,     &
!!     &          scalar_bc, field_name )
!!        character(len=kchara), intent(in) :: field_name
!!        integer(kind = kint), intent(in) :: i
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(group_data),          intent(in) :: nod_grp
!!        integer(kind = kint), intent(inout) :: ii
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!!      subroutine set_vect_nod_bc_type_from_data(ii, i, nd,            &
!!     &          IO_bc, nod_grp, vect_bc, field_name )
!!        character(len=kchara), intent(in) :: field_name
!!        integer(kind = kint), intent(in) :: i, nd
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(group_data), intent(in) :: nod_grp
!!        integer(kind = kint), intent(inout) :: ii
!!        type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!!      subroutine set_magne_nod_bc_type_from_sph(ii, i, nod_grp,       &
!!     &          magne_bc )
!!        integer(kind = kint), intent(in) :: i
!!        type(group_data),          intent(in) :: nod_grp
!!
!!        integer(kind = kint), intent(inout) :: ii(3)
!!        type(vect_fixed_nod_bc_type), intent(inout) :: magne_bc
!!      subroutine set_mag_p_nod_bc_type_from_sph(ii, i, nod_grp,       &
!!     &          scalar_bc )
!!        integer(kind = kint), intent(in) :: i
!!        type(group_data),          intent(in) :: nod_grp
!!        integer(kind = kint), intent(inout) :: ii
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      module set_nodal_bc_type
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
      subroutine set_nod_bc_type_from_ctl(ii, i, nod_grp,               &
     &          scalar_bc, bc_magnitude)
!
      use t_group_data
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: i
      real ( kind = kreal), intent(in) :: bc_magnitude
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      integer(kind = kint) :: k, inod
!
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
        ii=ii+1
        scalar_bc%ibc_id(ii) = nod_grp%item_grp(k)
        scalar_bc%bc_apt(ii) = bc_magnitude
      end do
!
      if ( bc_magnitude .ne. 0.0d0 ) then
        do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
          inod = nod_grp%item_grp(k)
          scalar_bc%ibc(inod) = 1
        end do
      end if
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
         inod = nod_grp%item_grp(k)
         scalar_bc%ibc2(inod) = 1
      end do
!
      end subroutine set_nod_bc_type_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_vect_nod_bc_type_from_ctl(ii, i, nd, nod_grp,      &
     &          vect_bc, bc_magnitude )
!
      use t_group_data
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: i, nd
      real ( kind = kreal), intent(in) :: bc_magnitude
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!
      integer(kind = kint) :: k, inod
!
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
        ii(nd) = ii(nd) + 1
        vect_bc%ibc_id(ii(nd),nd) = nod_grp%item_grp(k)
!        vect_bc%bc_apt(ii(nd),nd) = bc_magnitude
      end do
!
      if ( bc_magnitude .ne. 0.0d0 ) then
        do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
          inod = nod_grp%item_grp(k)
          vect_bc%ibc(inod,nd) = 1
        end do
      end if
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
         inod = nod_grp%item_grp(k)
         vect_bc%ibc2(inod,nd) = 1
      end do
!
      end subroutine set_vect_nod_bc_type_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_rot_nod_bc_type_from_ctl(ii, i, nd, nod_grp,       &
     &          rotation, bc_magnitude )
!
      use t_group_data
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: i, nd
      real ( kind = kreal), intent(in) :: bc_magnitude
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii(3)
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotation
!
      integer(kind = kint) :: k, inod
!
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
        ii(nd) = ii(nd) + 1
        rotation%ibc_id(ii(nd)) = nod_grp%item_grp(k)
        rotation%bc_rot_apt(ii(nd),nd) = bc_magnitude
      end do
!
      if ( bc_magnitude .ne. 0.0d0 ) then
        do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
          inod = nod_grp%item_grp(k)
          rotation%ibc(inod) = 1
        end do
      end if
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
         inod = nod_grp%item_grp(k)
         rotation%ibc2(inod) = 1
      end do
!
      end subroutine set_rot_nod_bc_type_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_bc_type_from_data(ii, i, IO_bc, nod_grp,       &
     &          scalar_bc, field_name )
!
      use t_group_data
      use t_nodal_bc_data
      use t_boundary_field_IO
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i
!
      type(IO_boundary), intent(in) :: IO_bc
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      integer(kind = kint) :: k, ia, inod
!
!
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_nod_grp) then
          if (IO_bc%group_name(ia) .eq. nod_grp%grp_name(i)             &
     &        .and. IO_bc%field_type(ia) .eq. field_name ) then
!
            do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
              ii = ii + 1
              inod = nod_grp%item_grp(k)
!
              scalar_bc%ibc_id(ii) = inod
!
              scalar_bc%ibc( inod) = 1
              scalar_bc%ibc2(inod) = 1
            end do
!
          end if
        end if
      end do
!
      end subroutine set_nod_bc_type_from_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_vect_nod_bc_type_from_data(ii, i, nd,              &
     &          IO_bc, nod_grp, vect_bc, field_name )
!
      use t_group_data
      use t_nodal_bc_data
      use t_boundary_field_IO
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i, nd
!
      type(IO_boundary), intent(in) :: IO_bc
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!
      integer(kind = kint) :: k, ia, inod
!
!
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_nod_grp) then
          if ( IO_bc%group_name(ia) .eq. nod_grp%grp_name(i)            &
     &        .and. IO_bc%field_type(ia) .eq. field_name ) then
!
            do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
              ii(nd) = ii(nd) + 1
              inod = nod_grp%item_grp(k)
!
              vect_bc%ibc_id(ii(nd),nd) = inod
!
              vect_bc%ibc( inod,nd) = 1
              vect_bc%ibc2(inod,nd) = 1
            end do
!
          end if
        end if
      end do
!
      end subroutine set_vect_nod_bc_type_from_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_magne_nod_bc_type_from_sph(ii, i, nod_grp,         &
     &          magne_bc )
!
      use t_group_data
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: i
      type(group_data),          intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: magne_bc
!
      integer(kind = kint) :: k, inod, nd
!
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
        inod = nod_grp%item_grp(k)
!
        do nd = 1, 3
          ii(nd) = ii(nd) + 1
          magne_bc%ibc_id(ii(nd),nd) = inod
        end do
!
        do nd = 1, 3
          magne_bc%ibc( inod,nd) = 1
          magne_bc%ibc2(inod,nd) = 1
        end do
!
      end do
!
      end subroutine set_magne_nod_bc_type_from_sph
!
!  ---------------------------------------------------------------------
!
      subroutine set_mag_p_nod_bc_type_from_sph(ii, i, nod_grp,         &
     &          scalar_bc )
!
      use t_group_data
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: i
      type(group_data),          intent(in) :: nod_grp
!
      integer(kind = kint), intent(inout) :: ii
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      integer(kind = kint) :: k, inod
!
!
      do k = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
        ii = ii + 1
        inod = nod_grp%item_grp(k)
!
        scalar_bc%ibc_id(ii) = inod
!
        scalar_bc%ibc( inod) = 1
        scalar_bc%ibc2(inod) = 1
      end do
!
      end subroutine set_mag_p_nod_bc_type_from_sph
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_bc_type
