!filter_IO_for_newdomain
!      module filter_IO_for_newdomain
!
!     Written by H. Matsui on May, 2008
!
!      subroutine write_new_whole_filter_coef(file_name)
!      subroutine write_new_fluid_filter_coef(file_name)
!      subroutine read_filter_coef_4_newdomain(id_file)
!      subroutine read_filter_coef_4_newdomain_b
!
      module filter_IO_for_newdomain
!
      use m_precision
!
      use m_nod_filter_comm_table
      use m_filter_coefs
      use m_filter_func_4_sorting
      use filter_IO_for_sorting
      use write_filters_4_each_node
!
      implicit none
!
      integer(kind = kint), allocatable :: inod_near_nod_tmp(:)
      real(kind = kreal), allocatable :: filter_func_tmp(:)
      real(kind = kreal), allocatable :: filter_weight_tmp(:)
!
      private :: inod_near_nod_tmp
      private :: filter_func_tmp, filter_weight_tmp
!
      private :: set_w_filter_item_4_IO, set_f_filter_item_4_IO
      private :: set_w_filter_item_4_newdomain
      private :: set_f_filter_item_4_newdomain
      private :: allocate_filter_coef_tmp, deallocate_filter_coef_tmp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_new_whole_filter_coef(file_name)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint) :: inod
!
!
      do inod = 1, inter_nod_3dfilter
        call set_w_filter_item_4_IO(inod)
        call write_each_filter_stack_coef(file_name, inod)
      end do
!
      end subroutine write_new_whole_filter_coef
!
!  ---------------------------------------------------------------------
!
      subroutine write_new_fluid_filter_coef(file_name)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint) :: inod
!
!
      do inod = 1, inter_nod_3dfilter
        call set_f_filter_item_4_IO(inod)
        if (nnod_near_1nod_weight .lt. 0) then
          nnod_near_1nod_weight = -nnod_near_1nod_weight
          call write_each_same_filter_coef(file_name, inod)
        else if (nnod_near_1nod_weight .eq. 0) then
          call write_each_no_filter_coef(file_name, inod)
        else
          call write_each_filter_stack_coef(file_name, inod)
        end if
      end do
!
      end subroutine write_new_fluid_filter_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_newdomain(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod
!
!
      ntot_nod_near_w_filter = 0
      call allocate_whole_filter_coefs
      call allocate_whole_filter_stack(inter_nod_3dfilter)
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each(id_file)
        call set_w_filter_item_4_newdomain(inod)
      end do
!
      ntot_nod_near_f_filter = 0
      call allocate_fluid_filter_stack(inter_nod_3dfilter)
      call allocate_fluid_filter_coefs
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each(id_file)
        call set_f_filter_item_4_newdomain(inod)
      end do
!
      end subroutine read_filter_coef_4_newdomain
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_newdomain_b
!
      integer(kind = kint) :: inod
!
!
      ntot_nod_near_w_filter = 0
      call allocate_whole_filter_coefs
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each_b
        call set_w_filter_item_4_newdomain(inod)
      end do
!
!
      ntot_nod_near_f_filter = 0
      call allocate_fluid_filter_coefs
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each_b
        call set_f_filter_item_4_newdomain(inod)
      end do
!
      end subroutine read_filter_coef_4_newdomain_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_w_filter_item_4_IO(inod)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: i, j
!
!
      i_exp_level_1nod_weight = i_exp_level_w_filter(inod)
      nnod_near_1nod_weight =   nnod_near_nod_w_filter(inod)
      do i = 1, nnod_near_1nod_weight
        j = istack_near_nod_w_filter(inod-1) + i
        inod_near_1nod_weight(i) = inod_near_nod_w_filter(j)
        filter_1nod(i) =           whole_filter_func(j)
        weight_1nod(i) = whole_filter_weight(j)
      end do
!
      end subroutine set_w_filter_item_4_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_f_filter_item_4_IO(inod)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: i, j
!
!
      i_exp_level_1nod_weight = i_exp_level_f_filter(inod)
      nnod_near_1nod_weight =   nnod_near_nod_f_filter(inod)
      do i = 1, nnod_near_1nod_weight
        j = istack_near_nod_f_filter(inod-1) + i
        inod_near_1nod_weight(i) = inod_near_nod_f_filter(j)
        filter_1nod(i) =           fluid_filter_func(j)
        weight_1nod(i) =           fluid_filter_weight(j)
      end do
!
      end subroutine set_f_filter_item_4_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_w_filter_item_4_newdomain(inod)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: i, j
!
!
      i_exp_level_w_filter(inod) =   i_exp_level_1nod_weight
      nnod_near_nod_w_filter(inod) = nnod_near_1nod_weight
      istack_near_nod_w_filter(inod)                                    &
     &       = istack_near_nod_w_filter(inod-1) + nnod_near_1nod_weight
!
      call allocate_filter_coef_tmp(ntot_nod_near_w_filter)
!
      do j = 1, ntot_nod_near_w_filter
        inod_near_nod_tmp(j) = inod_near_nod_w_filter(j)
        filter_func_tmp(j) =    whole_filter_func(j)
        filter_weight_tmp(j) =  whole_filter_weight(j)
      end do
!
      call deallocate_whole_filter_func
!
      ntot_nod_near_w_filter = istack_near_nod_w_filter(inod)
      call allocate_whole_filter_coefs
!
      do j = 1, istack_near_nod_w_filter(inod-1)
        inod_near_nod_w_filter(j) = inod_near_nod_tmp(j)
        whole_filter_func(j) =  filter_func_tmp(j)
        whole_filter_weight(j) = filter_weight_tmp(j)
      end do
!
      do i = 1, nnod_near_1nod_weight
        j = istack_near_nod_w_filter(inod-1) + i
        inod_near_nod_w_filter(j) = inod_near_1nod_weight(i)
        whole_filter_func(j) =   filter_1nod(i)
        whole_filter_weight(j) = weight_1nod(i)
      end do
!
      call deallocate_filter_coef_tmp
!
      end subroutine set_w_filter_item_4_newdomain
!
!  ---------------------------------------------------------------------
!
      subroutine set_f_filter_item_4_newdomain(inod)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: i, j
!
!
      i_exp_level_f_filter(inod) =   i_exp_level_1nod_weight
      nnod_near_nod_f_filter(inod) = nnod_near_1nod_weight
!
      if (nnod_near_1nod_weight .le. 0) then
        istack_near_nod_f_filter(inod)                                  &
     &       = istack_near_nod_f_filter(inod-1)
      else
        istack_near_nod_f_filter(inod)                                  &
     &       = istack_near_nod_f_filter(inod-1) + nnod_near_1nod_weight
!
        call allocate_filter_coef_tmp(ntot_nod_near_f_filter)
!
        do j = 1, ntot_nod_near_f_filter
          inod_near_nod_tmp(j) = inod_near_nod_f_filter(j)
          filter_func_tmp(j) =    fluid_filter_func(j)
          filter_weight_tmp(j) =  fluid_filter_weight(j)
        end do
!
        call deallocate_fluid_filter_func
!
        ntot_nod_near_f_filter = istack_near_nod_f_filter(inod)
        call allocate_fluid_filter_coefs
!
        do j = 1, istack_near_nod_f_filter(inod-1)
          inod_near_nod_f_filter(j) = inod_near_nod_tmp(j)
          fluid_filter_func(j) =   filter_func_tmp(j)
          fluid_filter_weight(j) = filter_weight_tmp(j)
        end do
!
        do i = 1, nnod_near_1nod_weight
          j = istack_near_nod_f_filter(inod-1) + i
          inod_near_nod_f_filter(j) = inod_near_1nod_weight(i)
          fluid_filter_func(j) =   filter_1nod(i)
          fluid_filter_weight(j) = weight_1nod(i)
        end do
!
        call deallocate_filter_coef_tmp
      end if
!
      end subroutine set_f_filter_item_4_newdomain
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_coef_tmp(num)
!
      integer(kind = kint), intent(in) :: num
!
      allocate( inod_near_nod_tmp(num) )
      allocate( filter_func_tmp(num) )
      allocate( filter_weight_tmp(num) )
      inod_near_nod_tmp = 0
      filter_func_tmp =   0.0d0
      filter_weight_tmp = 0.0d0
!
      end subroutine allocate_filter_coef_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_coef_tmp
!
      deallocate( inod_near_nod_tmp )
      deallocate( filter_func_tmp, filter_weight_tmp )
!
      end subroutine deallocate_filter_coef_tmp
!
!  ---------------------------------------------------------------------
!
      end module filter_IO_for_newdomain
