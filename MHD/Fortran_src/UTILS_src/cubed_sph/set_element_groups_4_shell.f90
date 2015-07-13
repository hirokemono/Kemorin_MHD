!
!     module set_element_groups_4_shell
!
!     Written by H. Matsui on Apr, 2006
!
!      subroutine write_element_group(id_file)
!      subroutine set_element_group_names
!      subroutine count_ele_groups
!      subroutine set_ele_grp_istack
!      subroutine set_ele_item
!
      module set_element_groups_4_shell
!
      use m_precision
!
      use m_cubed_sph_grp_param
      use m_element_group
      use m_cubed_sph_radius
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_element_group(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i, k, ist, ied
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 4.2. Element group'
!
      write(id_file,'(i16)') ele_grp1%num_grp
      write(id_file,'(6i16)')  (mat_istack(i),i=1,ele_grp1%num_grp)
!
      do k = 1, ele_grp1%num_grp
        ist = mat_istack(k-1)+1
        ied = mat_istack(k)
        write(id_file,*) trim(ele_grp1%grp_name(k))
        write(id_file,'(6i16)') (mat_item(i),i=ist,ied)
      end do
!
      end subroutine write_element_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_element_group_names
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_ele_grp_csp
        ele_grp1%grp_name(i) = ele_grp_name_csp(i)
      end do
!
      end subroutine set_element_group_names
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_ele_groups(numele_cube, numele_sf, nskip_r)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
      integer(kind = kint) :: i
!
      ele_grp1%num_grp = num_ele_grp_csp
!
      ele_grp1%num_item = 0
      do i = 1, num_ele_layer_csp
        if      (id_ele_grp_layer_csp(i) .eq. 0) then
          ele_grp1%num_item = ele_grp1%num_item + numele_cube
        else if (id_ele_grp_layer_csp(i) .gt. 0) then
          if ( mod(id_ele_grp_layer_csp(i),nskip_r) .eq. 0 ) then
            ele_grp1%num_item = ele_grp1%num_item + numele_sf
          end if
        end if
      end do
!
      end subroutine count_ele_groups
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_grp_istack(numele_cube, numele_sf, nskip_r)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
      integer(kind = kint) :: i, ist, ied, inum
!
!
      mat_istack(0) = 0
      do i = 1, num_ele_grp_csp
        mat_istack(i) = mat_istack(i-1)
        ist = istack_ele_grp_layer_csp(i-1) + 1
        ied = istack_ele_grp_layer_csp(i)
        do inum = ist, ied
          if      (id_ele_grp_layer_csp(inum) .eq. 0) then
            mat_istack(i) = mat_istack(i) + numele_cube
          else if (id_ele_grp_layer_csp(inum) .gt. 0) then
            if ( mod(id_ele_grp_layer_csp(inum),nskip_r) .eq. 0 ) then
              mat_istack(i) = mat_istack(i) + numele_sf
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
      subroutine set_ele_item(numele_cube, numele_sf, nskip_r)
!
      integer(kind = kint), intent(in) :: numele_cube, numele_sf
      integer(kind = kint), intent(in) :: nskip_r
      integer(kind = kint) :: i, ist, ied, inum, icou, iele
!
!
      do i = 1, num_ele_grp_csp
        icou = mat_istack(i-1)
!
        ist = istack_ele_grp_layer_csp(i-1) + 1
        ied = istack_ele_grp_layer_csp(i)
        do inum = ist, ied
          if      (id_ele_grp_layer_csp(inum) .eq. 0) then
            do iele = 1, numele_cube
              icou = icou + 1
              mat_item(icou) = iele
            end do
          else if (id_ele_grp_layer_csp(inum) .gt. 0) then
            if ( mod(id_ele_grp_layer_csp(inum),nskip_r) .eq. 0 ) then
              do iele = 1, numele_sf
                icou = icou + 1
                mat_item(icou) = iele + numele_cube + numele_sf         &
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
 