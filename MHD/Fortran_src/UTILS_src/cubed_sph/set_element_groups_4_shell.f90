!
!     module set_element_groups_4_shell
!
!     Written by H. Matsui on Apr, 2006
!
!      subroutine write_element_group(id_file, ele_grp)
!      subroutine set_element_group_names(ele_grp)
!      subroutine count_ele_groups                                      &
!     &          (numele_cube, numele_sf, nskip_r, ele_grp)
!      subroutine set_ele_grp_istack                                    &
!     &         (numele_cube, numele_sf, nskip_r, ele_grp)
!      subroutine set_ele_item                                          &
!     &         (numele_cube, numele_sf, nskip_r, ele_grp)
!
      module set_element_groups_4_shell
!
      use m_precision
!
      use m_cubed_sph_grp_param
      use m_cubed_sph_radius
      use t_group_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_element_group(id_file, ele_grp)
!
      integer(kind = kint), intent(in) :: id_file
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: i, k, ist, ied
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 4.2. Element group'
!
      write(id_file,'(i16)') ele_grp%num_grp
      write(id_file,'(6i16)')                                           &
     &         (ele_grp%istack_grp(i),i=1,ele_grp%num_grp)
!
      do k = 1, ele_grp%num_grp
        ist = ele_grp%istack_grp(k-1)+1
        ied = ele_grp%istack_grp(k)
        write(id_file,*) trim(ele_grp%grp_name(k))
        write(id_file,'(6i16)') (ele_grp%item_grp(i),i=ist,ied)
      end do
!
      end subroutine write_element_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_element_group_names(ele_grp)
!
      type(group_data), intent(inout) :: ele_grp
      integer(kind = kint) :: i
!
!
      do i = 1, num_ele_grp_csp
        ele_grp%grp_name(i) = ele_grp_name_csp(i)
      end do
!
      end subroutine set_element_group_names
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_ele_groups                                       &
     &          (numele_cube, numele_sf, nskip_r, ele_grp)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: i
!
      ele_grp%num_grp = num_ele_grp_csp
!
      ele_grp%num_item = 0
      do i = 1, num_ele_layer_csp
        if      (id_ele_grp_layer_csp(i) .eq. 0) then
          ele_grp%num_item = ele_grp%num_item + numele_cube
        else if (id_ele_grp_layer_csp(i) .gt. 0) then
          if ( mod(id_ele_grp_layer_csp(i),nskip_r) .eq. 0 ) then
            ele_grp%num_item = ele_grp%num_item + numele_sf
          end if
        end if
      end do
!
      end subroutine count_ele_groups
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_grp_istack                                     &
     &         (numele_cube, numele_sf, nskip_r, ele_grp)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: i, ist, ied, inum
!
!
      ele_grp%istack_grp(0) = 0
      do i = 1, num_ele_grp_csp
        ele_grp%istack_grp(i) = ele_grp%istack_grp(i-1)
        ist = istack_ele_grp_layer_csp(i-1) + 1
        ied = istack_ele_grp_layer_csp(i)
        do inum = ist, ied
          if      (id_ele_grp_layer_csp(inum) .eq. 0) then
            ele_grp%istack_grp(i) = ele_grp%istack_grp(i)               &
     &                              + numele_cube
          else if (id_ele_grp_layer_csp(inum) .gt. 0) then
            if ( mod(id_ele_grp_layer_csp(inum),nskip_r) .eq. 0 ) then
              ele_grp%istack_grp(i) = ele_grp%istack_grp(i)             &
     &                                + numele_sf
            end if
          end if
        end do
      end do
!
      end subroutine set_ele_grp_istack
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_item                                           &
     &         (numele_cube, numele_sf, nskip_r, ele_grp)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: i, ist, ied, inum, icou, iele
!
!
      do i = 1, num_ele_grp_csp
        icou = ele_grp%istack_grp(i-1)
!
        ist = istack_ele_grp_layer_csp(i-1) + 1
        ied = istack_ele_grp_layer_csp(i)
        do inum = ist, ied
          if      (id_ele_grp_layer_csp(inum) .eq. 0) then
            do iele = 1, numele_cube
              icou = icou + 1
              ele_grp%item_grp(icou) = iele
            end do
          else if (id_ele_grp_layer_csp(inum) .gt. 0) then
            if ( mod(id_ele_grp_layer_csp(inum),nskip_r) .eq. 0 ) then
              do iele = 1, numele_sf
                icou = icou + 1
                ele_grp%item_grp(icou) = iele                           &
     &                       + numele_cube + numele_sf                  &
     &                        * (id_ele_grp_layer_csp(inum)/nskip_r-1)
              end do
            end if
          end if
        end do
!
      end do
!
      end subroutine set_ele_item
!
!  ---------------------------------------------------------------------
!
      end module set_element_groups_4_shell
 