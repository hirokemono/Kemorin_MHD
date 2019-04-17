!write_ele_grp_cube.f90
!     module write_ele_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine const_element_group                                  &
!!     &         (c_size, c_each, nb_rng, kpe, ele_grp)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type (group_data), intent(inout) :: ele_grp
!
      module write_ele_grp_cube
!
      use m_precision
!
      use t_group_data
      use t_size_of_cube
      use t_neib_range_cube
!
      implicit none
!
      character(len=kchara), parameter :: group_head = 'fluid_layer_'
      integer(kind=kint), allocatable  :: iele_group_id(:)
!
      private :: group_head, iele_group_id
      private :: allocate_cube_ele_group_id
      private :: deallocate_cube_ele_group_id
      private :: count_element_group, set_cube_ele_group
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_element_group                                    &
     &         (c_size, c_each, nb_rng, kpe, ele_grp)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      integer(kind = kint), intent(in) :: kpe
!
      type (group_data), intent(inout) :: ele_grp
!
!
      ele_grp%num_grp = 3 + (c_size%nz_all-1)
      call alloc_group_num(ele_grp)
!
      call allocate_cube_ele_group_id(c_size)
      call count_element_group(c_size, c_each%elm_fil1_tot, ele_grp)
!
      call set_cube_ele_group(c_size, c_each%nx, c_each%ny, c_each%nz,  &
     &    kpe, nb_rng%koff, ele_grp)
      call deallocate_cube_ele_group_id
!
      end subroutine const_element_group
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_cube_ele_group_id(c_size)
!
      type(size_of_cube), intent(in) :: c_size
!
!
      allocate(iele_group_id(2*c_size%nnod_cubmesh))
      if(c_size%nnod_cubmesh .gt. 0) iele_group_id = 0
!
      end subroutine allocate_cube_ele_group_id
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_cube_ele_group_id
!
       deallocate(iele_group_id)
!
       end subroutine deallocate_cube_ele_group_id
!
! ---------------------------------------------------------------------
!
      subroutine count_element_group(c_size, elm_fil1_tot, ele_grp)
!
      type(size_of_cube), intent(in) :: c_size
      integer (kind=kint), intent(in) :: elm_fil1_tot
!
      type (group_data), intent(inout) :: ele_grp
!
       integer(kind = kint) :: item_tot
       integer(kind = kint) :: item_pos
!
       item_tot = 0
       item_pos = 0
       ele_grp%istack_grp = 0
!                                                 .. all
!
       item_pos = 1
       item_tot = item_tot + (c_size%nz_all - 1)
       ele_grp%istack_grp(item_pos) = item_tot
       ele_grp%grp_name(item_pos) = 'layer_start'
!
       item_pos = 2
       item_tot = item_tot + (c_size%nz_all - 1)
       ele_grp%istack_grp(item_pos) = item_tot
       ele_grp%grp_name(item_pos) = 'layer_end'
!
       item_pos = 3
       item_tot = item_tot + elm_fil1_tot
       ele_grp%istack_grp(item_pos) = item_tot
       ele_grp%grp_name(item_pos) = 'conductive_fluid'
!
       end subroutine count_element_group
!
!-----------------------------------------------------------------------
!
      subroutine set_cube_ele_group                                     &
     &         (c_size, nx, ny, nz, kpe, koff, ele_grp)
!
      use set_parallel_file_name
!
      type(size_of_cube), intent(in) :: c_size
      integer (kind=kint), intent(in) :: nx, ny, nz
      integer(kind = kint), intent(in) :: kpe
      integer(kind=kint), intent(in) :: koff
!
      type (group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: iele, element_id
      integer(kind = kint) :: i, j, k, item_pos, item_tot
!
!
       iele = 0
       element_id = 1
       do k = 1, c_size%nz_all-1
        iele = iele + 1
        iele_group_id(iele) = element_id
        if(k.gt.koff .and. k.le.(koff+nz-1) ) then
         element_id = element_id + (nx-1)*(ny-1)
        end if
       end do
!
       element_id = 0
       do k = 1, c_size%nz_all-1
        if ( k.gt.koff .and. k.le.(koff+nz-1) ) then
         element_id = element_id + (nx-1)*(ny-1)
        end if
        iele = iele + 1
        iele_group_id(iele) = element_id
       end do
!
       element_id = 0
       do k=1,nz-1
        do j=1,ny-1
         do i=1,nx-1

           element_id    =  element_id + 1
           if(i.ge.c_size%ndepth .and. i.le.(nx-c_size%ndepth)) then
            if(j.ge.c_size%ndepth .and. j.le.(ny-c_size%ndepth)) then
             if(kpe.eq.1 .and. k.le.(nz-c_size%ndepth) ) then
              iele = iele + 1
              iele_group_id(iele) = element_id
             else if(kpe.eq.c_size%ndz .and. k.ge.c_size%ndepth) then
              iele = iele + 1
              iele_group_id(iele) = element_id
             else if(k.ge.c_size%ndepth                                 &
     &              .and. k.le.(nz-c_size%ndepth) ) then
              iele = iele + 1
              iele_group_id(iele) = element_id
             end if
            end if
           end if
!
           end do
         end do
       end do
!
!       write(*,*) 'nz_all gc',                                         &
!     &           c_size%nx_all, c_size%ny_all, c_size%nz_all
       item_tot = ele_grp%istack_grp(3)
       do k = 1, (c_size%nz_all-1)
         j = k + ele_grp%istack_grp(1)
         item_pos = 3 + k
         item_tot = item_tot + 1                                        &
     &             + iele_group_id(j) - iele_group_id(k)
         ele_grp%istack_grp(item_pos) = item_tot
!
         call add_index_after_name                                      &
      &     (k, group_head, ele_grp%grp_name(k+3))
       end do
!
       do k = 1, (c_size%nz_all-1)
         j = k + ele_grp%istack_grp(1)
         do i = iele_group_id(k), iele_group_id(j)
           iele = iele + 1
           iele_group_id(iele) = i
         end do
       end do
!
       ele_grp%num_item = ele_grp%istack_grp(ele_grp%num_grp)
       call alloc_group_item(ele_grp)
!
!$omp parallel workshare
       ele_grp%item_grp(1:ele_grp%num_item)                             &
      &      =  iele_group_id(1:ele_grp%num_item)
!$omp end parallel workshare
!
       end subroutine set_cube_ele_group
!
! ----------------------------------------------------------------------
!
      end module write_ele_grp_cube
