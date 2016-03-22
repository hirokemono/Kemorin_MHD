!
!      module filter_IO_for_sorting
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine read_filter_neib_4_sort(id_file)
!      subroutine read_filter_neib_4_sort_b(id_file)
!
!      subroutine read_filter_coef_4_sort(id_file)
!      subroutine read_filter_coef_4_sort_b(id_file, filter)
!
!      subroutine write_filter_coef_4_each(id_file)
!      subroutine write_filter_coef_4_each_b(id_file)
!
!      subroutine read_filter_coef_4_each(id_file)
!      subroutine read_filter_coef_4_each_b(id_file)
!
      module filter_IO_for_sorting
!
      use m_precision
!
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_func_4_sorting
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
      private :: set_filtering_item_4_sorting
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_neib_4_sort(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod, itmp, jnod
!
!
      call allocate_filter_num_sort_IO
!
      call allocate_whole_filter_stack(inter_nod_3dfilter)
      call allocate_fluid_filter_stack(inter_nod_3dfilter)
!
      nmax_nod_near_all_w = 0
      nmax_ele_near_all_w = 0
      do inod = 1, inter_nod_3dfilter
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) itmp, nnod_near_nod_w_filter(inod),    &
     &                          i_exp_level_whole_nod(inod)
        nmax_nod_near_all_w                                             &
     &        = max(nmax_nod_near_all_w,nnod_near_nod_w_filter(inod))
        do jnod = 1, nnod_near_nod_w_filter(inod)
          read(id_file,*) itmp
        end do
      end do
!
      do inod = 1, inter_nod_3dfilter
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) itmp, nnod_near_nod_f_filter(inod),    &
     &                          i_exp_level_fluid_nod(inod)
        nmax_nod_near_all_w                                             &
     &        = max(nmax_nod_near_all_w,nnod_near_nod_f_filter(inod))
        do jnod = 1, nnod_near_nod_f_filter(inod)
          read(id_file,*) itmp
        end do
      end do
!
      end subroutine read_filter_neib_4_sort
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_neib_4_sort_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod, itmp, jnod
!
!
      nmax_nod_near_all_w = 0
      nmax_ele_near_all_w = 0
      call allocate_filter_num_sort_IO
      call allocate_whole_filter_stack(inter_nod_3dfilter)
      call allocate_fluid_filter_stack(inter_nod_3dfilter)
!
      do inod = 1, inter_nod_3dfilter
        read(id_file) itmp, nnod_near_nod_w_filter(inod),               &
     &                          i_exp_level_whole_nod(inod)
        nmax_nod_near_all_w                                             &
     &        = max(nmax_nod_near_all_w,nnod_near_nod_w_filter(inod))
        read(id_file) (itmp,jnod=1,nnod_near_nod_w_filter(inod))
      end do
!
      do inod = 1, inter_nod_3dfilter
        read(id_file) itmp, nnod_near_nod_f_filter(inod),               &
     &                          i_exp_level_fluid_nod(inod)
        nmax_nod_near_all_w                                             &
     &        = max(nmax_nod_near_all_w,nnod_near_nod_f_filter(inod))
        read(id_file) (itmp,jnod=1,nnod_near_nod_f_filter(inod))
      end do
!
      end subroutine read_filter_neib_4_sort_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_sort(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod, icou, itmp
!
!
      do inod = 1, inter_nod_3dfilter
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) itmp, nnod_near_1nod_weight, itmp
!
        call read_filter_coef_4_each(id_file)
!
        icou = itbl_near_nod_whole(inod)
        call set_filtering_item_4_sorting(icou)
      end do
!
        do inod = 1, inter_nod_3dfilter
          call skip_comment(character_4_read,id_file)
          read(character_4_read,*) itmp, nnod_near_1nod_weight, itmp
!
          call read_filter_coef_4_each(id_file)
!
          icou = itbl_near_nod_fluid(inod)
          call set_filtering_item_4_sorting(icou)
!
        end do
!
      end subroutine read_filter_coef_4_sort
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_sort_b(id_file, filter)
!
      use t_filter_coefficients
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: filter
      integer(kind = kint) :: inod, icou
!
!
      do inod = 1, inter_nod_3dfilter
        read(id_file) nnod_near_1nod_weight, i_exp_level_1nod_weight
        call read_filter_coef_4_each_b(id_file)
!
        icou = itbl_near_nod_whole(inod)
        call set_filtering_item_4_sorting(icou)
!
      end do
!
      do inod = 1, inter_nod_3dfilter
        read(id_file) nnod_near_1nod_weight, i_exp_level_1nod_weight
        if (icou .le. filter%ntot_nod) then
          call read_filter_coef_4_each_b(id_file)
!
          icou = itbl_near_nod_fluid(inod)
          call set_filtering_item_4_sorting(icou)
        end if
      end do
!
      end subroutine read_filter_coef_4_sort_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_filtering_item_4_sorting(icou)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint) :: i, j, ist_nod
!
!
        ist_nod = inod_stack_nod_all_w(icou-1)
        do i = 1, nnod_near_1nod_weight
          j = ist_nod + i
          inod_near_nod_all_w(j) = inod_near_1nod_weight(i)
          filter_func(j) =   filter_1nod(i)
          filter_weight(j) = weight_1nod(i)
        end do
!
      end subroutine set_filtering_item_4_sorting
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_filter_coef_4_each(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inum
!
!
      if (nnod_near_1nod_weight .gt. 0) then
        do inum = 1, nnod_near_1nod_weight
          write(id_file,'(2i12,1p2E25.15e3)') inum,                     &
     &      inod_near_1nod_weight(inum), filter_1nod(inum),             &
     &      weight_1nod(inum)
        end do
      end if
!
      end subroutine write_filter_coef_4_each
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_coef_4_each_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      if (nnod_near_1nod_weight .gt. 0) then
        write(id_file) inod_near_1nod_weight(1:nnod_near_1nod_weight)
        write(id_file) filter_1nod(1:nnod_near_1nod_weight)
        write(id_file) weight_1nod(1:nnod_near_1nod_weight)
      end if
!
      end subroutine write_filter_coef_4_each_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_each(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inum, itmp
!
      if (nnod_near_1nod_weight.gt. 0) then
        do inum = 1, nnod_near_1nod_weight
          read(id_file,*) itmp,                                         &
     &      inod_near_1nod_weight(inum), filter_1nod(inum),             &
     &      weight_1nod(inum)
        end do
      end if
!
      end subroutine read_filter_coef_4_each
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_each_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      if (nnod_near_1nod_weight.gt. 0) then
        read(id_file) inod_near_1nod_weight(1:nnod_near_1nod_weight)
        read(id_file) filter_1nod(1:nnod_near_1nod_weight)
        read(id_file) weight_1nod(1:nnod_near_1nod_weight)
      end if
!
      end subroutine read_filter_coef_4_each_b
!
!  ---------------------------------------------------------------------
!
      end module filter_IO_for_sorting
