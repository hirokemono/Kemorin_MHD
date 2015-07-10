!
!     module set_surface_groups_4_shell
!
!     Written by H. Matsui on Apr, 2006
!
!      subroutine write_surface_group(id_file)
!      subroutine set_surface_group_names
!      subroutine count_surf_groups
!      subroutine set_surf_istack
!      subroutine set_surf_item
!
      module set_surface_groups_4_shell
!
      use m_precision
!
      use m_cubed_sph_grp_param
      use m_surface_group
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_surface_group(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i, k, ist, ied
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 4.2. Surface group'
!
      write(id_file,'(i16)') sf_grp1%num_grp
      write(id_file,'(6i16)')  sf_grp1%istack_grp(1:sf_grp1%num_grp)
!
      do k = 1, sf_grp1%num_grp
        ist = sf_grp1%istack_grp(k-1) + 1
        ied = sf_grp1%istack_grp(k)
        write(id_file,*) trim(surf_name(k))
        write(id_file,'(6i16)') (surf_item(1,i),i=ist,ied)
        write(id_file,'(6i16)') (surf_item(2,i),i=ist,ied)
      end do
!
      end subroutine write_surface_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_group_names
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_surf_grp_csp
        surf_name(i) = surf_grp_name_csp(i)
      end do
!
      end subroutine set_surface_group_names
!
!  ---------------------------------------------------------------------
!
      subroutine count_surf_groups(numele_sf, nskip_r)
!
      integer(kind = kint), intent(in) :: numele_sf, nskip_r
      integer(kind = kint) :: i, icou
!
!
      sf_grp1%num_grp = num_surf_grp_csp
!
      icou = 0
      do i = 1, num_surf_layer_csp
        if( mod(id_surf_grp_layer_csp(1,i),nskip_r) .eq. 0 ) then
          icou = icou + 1
        end if
      end do
!
      sf_grp1%num_item = icou * numele_sf
      write(*,*) 'num_surf_bc', sf_grp1%num_item, numele_sf
!
      end subroutine count_surf_groups
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_istack(numele_sf, nskip_r)
!
      integer(kind = kint), intent(in) :: numele_sf, nskip_r
!
      integer(kind = kint) :: i, ist, ied, inum
!
!
      sf_grp1%istack_grp(0) = 0
      do i = 1, num_surf_grp_csp
        sf_grp1%istack_grp(i) = sf_grp1%istack_grp(i-1)
        ist = istack_surf_grp_layer_csp(i-1) + 1
        ied = istack_surf_grp_layer_csp(i)
        do inum = ist, ied
          if( mod(id_surf_grp_layer_csp(1,inum),nskip_r) .eq. 0 ) then
            sf_grp1%istack_grp(i) = sf_grp1%istack_grp(i) + numele_sf
          end if
        end do
      end do
!
      end subroutine set_surf_istack
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_item(numele_cube, numele_sf, nskip_r)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
!
      integer(kind = kint) :: iele0, icou
      integer(kind = kint) :: i, j, ist, ied, inum
!
!
      do i = 1, num_surf_grp_csp
        icou = sf_grp1%istack_grp(i-1)
        ist = istack_surf_grp_layer_csp(i-1) + 1
        ied = istack_surf_grp_layer_csp(i)
        do inum = ist, ied
          if( mod(id_surf_grp_layer_csp(1,inum),nskip_r) .eq. 0 ) then
            iele0 = numele_cube + numele_sf                             &
     &             * (id_surf_grp_layer_csp(1,inum)/nskip_r-1)
            do j = 1, numele_sf
              icou = icou + 1
              surf_item(1,icou) = iele0 + j
              surf_item(2,icou) = id_surf_grp_layer_csp(2,inum)
            end do
          end if
        end do
      end do
!
      end subroutine set_surf_item
!
!  ---------------------------------------------------------------------
!
      end module set_surface_groups_4_shell
