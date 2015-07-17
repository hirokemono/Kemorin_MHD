!
!     module set_node_groups_4_shell
!
!     Written by H. Matsui on Apr, 2006
!
!!      subroutine write_cubed_sph_nod_grp(id_file, nod_grp)
!!      subroutine set_node_group_names(nod_grp)
!!
!!      subroutine count_node_groups_linear(numnod_cube, numnod_sf,     &
!!     &          nskip_r, nod_grp)
!!      subroutine count_node_groups_quad(numnod_cube, numedge_cube,    &
!!     &          numnod_sf, numedge_sf, nod_grp)
!
!!      subroutine set_node_istack_linear(numnod_cube, numnod_sf,       &
!!     &          nskip_r, nod_grp)
!!      subroutine set_node_istack_quad(numnod_cube, numedge_cube,      &
!!     &          numnod_sf, numedge_sf, nod_grp)
!
!!      subroutine set_nodal_item_linear(numnod_cube, numnod_sf,        &
!!     &          num_hemi, nskip_r, nod_grp)
!!      subroutine set_nodal_item_quad                                  &
!!     &         (numnod, numnod_cube, numedge_cube,                    &
!!     &          numnod_sf, numedge_sf, num_hemi, nskip_r, nod_grp)
!
      module set_node_groups_4_shell
!
      use m_precision
!
      use m_cubed_sph_grp_param
      use t_group_data
!
      implicit  none
!
      private :: set_nodal_item_center
      private :: set_nodal_item_sphere_l, set_nodal_item_sphere_q
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_cubed_sph_nod_grp(id_file, nod_grp)
!
      integer(kind = kint), intent(in) :: id_file
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint) :: k, ist, ied
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! boundary conditions'
      write(id_file,'(a)') '! 4. Group information'
      write(id_file,'(a)') '! 4.1. Node group'
!
      write(id_file,'(i16)') nod_grp%num_grp
      write(id_file,'(6i16)') nod_grp%istack_grp(1:nod_grp%num_grp)
!
      do k = 1, nod_grp%num_grp
        ist = nod_grp%istack_grp(k-1) + 1
        ied = nod_grp%istack_grp(k)
        write(id_file,*) trim(nod_grp%grp_name(k))
        write(id_file,'(6i16)') nod_grp%item_grp(ist:ied)
      end do
!
      end subroutine write_cubed_sph_nod_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_node_group_names(nod_grp)
!
      type(group_data), intent(inout) :: nod_grp
      integer(kind = kint) :: i
!
      nod_grp%grp_name(1) = 'Center'
      nod_grp%grp_name(2) = 'Press'
      do i = 1, num_node_grp_csp
        nod_grp%grp_name(i+2) = nod_grp_name_csp(i)
      end do
!
      end subroutine set_node_group_names
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_node_groups_linear(numnod_cube, numnod_sf,       &
     &          nskip_r, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numnod_sf
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i
!
!
      nod_grp%num_grp = 2 + num_node_grp_csp
!
      nod_grp%num_item = 2
      do i = 1, num_nod_layer_csp
        if      ( id_nod_grp_layer_csp(i) .eq. 0 ) then
          nod_grp%num_item = nod_grp%num_item + numnod_cube
        else if ( id_nod_grp_layer_csp(i) .gt. 0 ) then
          if ( mod(id_nod_grp_layer_csp(i),nskip_r) .eq. 0 ) then
            nod_grp%num_item = nod_grp%num_item + numnod_sf
          end if
        end if
      end do
!
      end subroutine count_node_groups_linear
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_groups_quad(numnod_cube, numedge_cube,      &
     &          numnod_sf, numedge_sf, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numedge_cube
      integer(kind = kint), intent(in) :: numnod_sf, numedge_sf
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i
!
      nod_grp%num_item = 2
      do i = 1, num_nod_layer_csp
        if      ( id_nod_grp_layer_csp(i) .eq. 0 ) then
          nod_grp%num_item = nod_grp%num_item                           &
     &                       + numnod_cube + numedge_cube
        else if ( id_nod_grp_layer_csp(i) .gt. 0 ) then
          nod_grp%num_item = nod_grp%num_item                           &
     &                       + numnod_sf + numedge_sf
        else if ( id_nod_grp_layer_csp(i) .lt. 0 ) then
          nod_grp%num_item = nod_grp%num_item + numnod_sf
        end if
      end do
!
      nod_grp%num_grp = 2 + num_node_grp_csp
!
      end subroutine count_node_groups_quad
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_istack_linear(numnod_cube, numnod_sf,         &
     &          nskip_r, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numnod_sf
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i, inum, ist, ied
!
!
      nod_grp%istack_grp(0) = 0
      nod_grp%istack_grp(1) = 1
      nod_grp%istack_grp(2) = nod_grp%istack_grp(1) + 1
      do i = 1, num_node_grp_csp
        nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+1)
        ist = istack_nod_grp_layer_csp(i-1) + 1
        ied = istack_nod_grp_layer_csp(i)
        do inum = ist, ied
          if      ( id_nod_grp_layer_csp(inum) .eq. 0 ) then
            nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+2)           &
     &                                + numnod_cube
          else if ( id_nod_grp_layer_csp(inum) .gt. 0 ) then
            if ( mod(id_nod_grp_layer_csp(inum),nskip_r) .eq. 0 ) then
              nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+2)         &
     &                                  + numnod_sf
            end if
          end if
        end do
      end do
!
      end subroutine set_node_istack_linear
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_istack_quad(numnod_cube, numedge_cube,        &
     &          numnod_sf, numedge_sf, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numedge_cube
      integer(kind = kint), intent(in) :: numnod_sf, numedge_sf
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i, inum, ist, ied
!
      nod_grp%istack_grp(0) = 0
      nod_grp%istack_grp(1) = 1
      nod_grp%istack_grp(2) = nod_grp%istack_grp(1) + 1
      do i = 1, num_node_grp_csp
        nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+1)
        ist = istack_nod_grp_layer_csp(i-1) + 1
        ied = istack_nod_grp_layer_csp(i)
        do inum = ist, ied
          if      ( id_nod_grp_layer_csp(inum) .eq. 0 ) then
            nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+2)           &
     &                                + numnod_cube + numedge_cube
          else if ( id_nod_grp_layer_csp(inum) .gt. 0 ) then
            nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+2)           &
     &                                + numnod_sf + numedge_sf
          else if ( id_nod_grp_layer_csp(inum) .lt. 0 ) then
            nod_grp%istack_grp(i+2) = nod_grp%istack_grp(i+2)           &
     &                                + numnod_sf
          end if
        end do
      end do
!
      end subroutine set_node_istack_quad
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nodal_item_linear(numnod_cube, numnod_sf,          &
     &          num_hemi, nskip_r, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numnod_sf
      integer(kind = kint), intent(in) :: num_hemi
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: nod_grp
!
!
      call set_nodal_item_center(numnod_cube, numnod_sf, nskip_r,       &
     &    num_hemi, nod_grp)
      call set_nodal_item_sphere_l                                      &
     &   (numnod_cube, numnod_sf, nskip_r, nod_grp)
!
      end subroutine set_nodal_item_linear
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodal_item_quad                                    &
     &         (numnod, numnod_cube, numedge_cube,                      &
     &          numnod_sf, numedge_sf, num_hemi, nskip_r, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numnod_sf
      integer(kind = kint), intent(in) :: numedge_cube, numedge_sf
      integer(kind = kint), intent(in) :: numnod, num_hemi
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: nod_grp
!
!
      call set_nodal_item_center(numnod_cube, numnod_sf, nskip_r,       &
     &    num_hemi, nod_grp)
      call set_nodal_item_sphere_q(numnod, numnod_cube, numedge_cube,   &
     &    numnod_sf, numedge_sf, nod_grp)
!
      end subroutine set_nodal_item_quad
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodal_item_center(numnod_cube, numnod_sf, nskip_r, &
     &          num_hemi, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numnod_sf
      integer(kind = kint), intent(in) :: num_hemi
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i
      integer(kind = kint) :: inod0
!
!     Center
!
      inod0 = (num_hemi-1)*(num_hemi-1)*(num_hemi-2) / 2                &
     &      + (num_hemi-1)*(num_hemi-2) / 2                             &
     &      + num_hemi / 2
      i = nod_grp%istack_grp(0) + 1
      nod_grp%item_grp(i) = inod0
!
!  North pole at CMB
!
      inod0 = numnod_cube + numnod_sf * (nr_cmb/nskip_r + 1)            &
     &      - (num_hemi+1)*(num_hemi) / 2 - num_hemi/2
      i = nod_grp%istack_grp(1) + 1
      nod_grp%item_grp(i) = inod0
!
      end subroutine set_nodal_item_center
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodal_item_sphere_l(numnod_cube, numnod_sf,        &
     &          nskip_r, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numnod_sf
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i, j, icou, inum, ist, ied
      integer(kind = kint) :: inod0
!
!
      do i = 1, num_node_grp_csp
!
        icou = nod_grp%istack_grp(i+1)
        ist = istack_nod_grp_layer_csp(i-1) + 1
        ied = istack_nod_grp_layer_csp(i)
!
        do inum = ist, ied
!
          if      (id_nod_grp_layer_csp(inum) .eq. 0) then
!
            do j = 1, numnod_cube
              icou = icou + 1
              nod_grp%item_grp(icou) = j
            end do
!
          else if (id_nod_grp_layer_csp(inum) .gt. 0) then
!
            if ( mod(id_nod_grp_layer_csp(inum),nskip_r) .eq. 0 ) then
              inod0 = numnod_cube + numnod_sf                           &
     &               * (id_nod_grp_layer_csp(inum)/nskip_r - 1)
              do j = 1, numnod_sf
                icou = icou + 1
                nod_grp%item_grp(icou) = inod0 + j
              end do
            end if
!
          end if
        end do
!
      end do
!
      end subroutine set_nodal_item_sphere_l
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodal_item_sphere_q(numnod, numnod_cube,           &
     &          numedge_cube, numnod_sf, numedge_sf, nod_grp)
!
      integer(kind = kint), intent(in) :: numnod_cube, numedge_cube
      integer(kind = kint), intent(in) :: numnod_sf, numedge_sf
      integer(kind = kint), intent(in) :: numnod
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: i, j, icou, inum, ist, ied
      integer(kind = kint) :: inod0, inod9, inod17
!
!
      do i = 1, num_node_grp_csp
!
        icou = nod_grp%istack_grp(i+1)
        ist = istack_nod_grp_layer_csp(i-1) + 1
        ied = istack_nod_grp_layer_csp(i)
!
        do inum = ist, ied
!
          if      (id_nod_grp_layer_csp(inum) .eq. 0) then
!
            do j = 1, numnod_cube
              icou = icou + 1
              nod_grp%item_grp(icou) = j
            end do
            do j = 1, numedge_cube
              icou = icou + 1
              nod_grp%item_grp(icou) = numnod + j
            end do
!
          else if (id_nod_grp_layer_csp(inum) .gt. 0) then
!
            inod0 = numnod_cube                                         &
     &             + numnod_sf * (id_nod_grp_layer_csp(inum)-1)
            inod9 = numnod + numedge_cube + numnod_sf                   &
     &                     + (numnod_sf+numedge_sf)                     &
     &                      * (id_nod_grp_layer_csp(inum)-2)
            do j = 1, numnod_sf
              icou = icou + 1
              nod_grp%item_grp(icou) = inod0 + j
            end do
            do j = 1, numedge_sf
              icou = icou + 1
              nod_grp%item_grp(icou) = inod9 + j
            end do
!
          else if (id_nod_grp_layer_csp(inum) .lt. 0) then
!
            inod17 = numnod + numedge_cube                              &
     &                      + (numnod_sf+numedge_sf)                    &
     &                      * (-id_nod_grp_layer_csp(inum)-1)
            do j = 1, numnod_sf
              icou = icou + 1
              nod_grp%item_grp(icou) = inod17 + j
            end do
!
          end if
        end do
!
      end do
!
      end subroutine set_nodal_item_sphere_q
!
!  ---------------------------------------------------------------------
!
      end module set_node_groups_4_shell
