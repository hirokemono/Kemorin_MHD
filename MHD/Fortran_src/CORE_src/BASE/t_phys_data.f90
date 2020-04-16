!>@file   t_phys_data.f90
!!        module t_phys_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!!      Modified March, 2020
!!
!> @brief Structure of field data
!!
!!@verbatim
!!      subroutine alloc_phys_name_type(fld)
!!      subroutine alloc_phys_data_type(num, fld)
!!        integer(kind = kint), intent(in) :: num
!!
!!      subroutine dealloc_phys_name_type(fld)
!!      subroutine dealloc_phys_data_type(fld)
!!
!!      subroutine append_field_name_list(iflag_add, field_name, numdir,&
!!     &          iflag_viz, flag_monitor, iorder_eletype, fld)
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine copy_field_name_type(org_fld, new_fld)
!!      subroutine copy_field_data_type(org_fld, new_fld)
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!
!!      integer(kind = kint) function field_id_by_address(fld, i_ref)
!!      character(len=kchara) function field_name_by_address(fld, i_ref)
!!        type(phys_data), intent(in) :: fld
!!
!!      subroutine check_all_field_data(id_rank, fld)
!!      subroutine check_nodal_field_name_type(id_output, fld)
!!      subroutine check_nodal_data(id_output, fld, numdir, i_field)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numdir, i_field
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module t_phys_data
!
      use m_precision
!
      use t_phys_address
!
      implicit  none
! 
!>       Structure for field data
      type phys_data
!>       number of data points
        integer (kind=kint) :: n_point
!>       number of field
        integer (kind=kint) :: num_phys
!>       total number of component
        integer (kind=kint) :: ntot_phys
!>       number of component for each field
        integer (kind=kint), allocatable :: num_component(:)
!>       end address for each field
        integer (kind=kint), allocatable :: istack_component(:)
!>       FEM order of each field
        integer (kind=kint), allocatable :: iorder_eletype(:)
!
!>       field name
        character (len=kchara), allocatable :: phys_name(:)
!
!>       field data
        real (kind=kreal), allocatable ::   d_fld(:,:)
!>       update flag for field data
        integer (kind=kint), allocatable :: iflag_update(:)
!
!>        number of field for visualizer
        integer (kind=kint) :: num_phys_viz
!>        total number of component for visualizer
        integer (kind=kint) :: ntot_phys_viz
!
!>        flag to get average and RMS data
        logical, allocatable:: flag_monitor(:)
      end type phys_data
!
      private :: add_field_name_list_4_viz, add_field_name_list_noviz
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_phys_name_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      allocate( fld%phys_name(fld%num_phys) )
      allocate( fld%num_component(fld%num_phys) )
      allocate( fld%istack_component(0:fld%num_phys) )
      allocate( fld%iorder_eletype(fld%num_phys) )
      allocate( fld%flag_monitor(fld%num_phys) )
!
      if(fld%num_phys .gt. 0) then
        fld%phys_name = ''
        fld%num_component =    0
        fld%istack_component = 0
        fld%iorder_eletype =   1
        fld%flag_monitor =  .FALSE.
      end if
!
      end subroutine alloc_phys_name_type
!
!  --------------------------------------------------------------------
!
      subroutine alloc_phys_data_type(num, fld)
!
      integer(kind = kint), intent(in) :: num
      type(phys_data), intent(inout) :: fld
!
!
      fld%n_point = num
      allocate( fld%d_fld(fld%n_point,fld%ntot_phys) )
      allocate( fld%iflag_update(fld%ntot_phys) )
      if(fld%n_point*fld%ntot_phys .gt. 0)  then
        fld%d_fld =        0.0d0
        fld%iflag_update = 0
      end if
!
       end subroutine alloc_phys_data_type
!
! --------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_phys_name_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      deallocate( fld%phys_name, fld%iorder_eletype, fld%flag_monitor)
      deallocate( fld%num_component, fld%istack_component )
!
      end subroutine dealloc_phys_name_type
!
!  --------------------------------------------------------------------
!
      subroutine dealloc_phys_data_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      deallocate( fld%d_fld, fld%iflag_update )
!
      end subroutine dealloc_phys_data_type
!
!  --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_field_name_list(iflag_add, field_name, numdir,  &
     &          iflag_viz, flag_monitor, iorder_eletype, fld)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: iflag_add, numdir
      integer(kind = kint), intent(in) :: iflag_viz
      logical, intent(in) :: flag_monitor
      integer(kind = kint), intent(in) :: iorder_eletype
!
      type(phys_data), intent(inout) :: fld
!
      type(phys_data) :: tmp_fld
!
!
      if(iflag_add .eq. 0) return
!
      call copy_field_name_type(fld, tmp_fld)
      call dealloc_phys_name_type(fld)
!
      fld%num_phys =      fld%num_phys + 1
      fld%ntot_phys =     fld%ntot_phys + numdir
!
      if(iflag_viz .gt. 0) then
        fld%num_phys_viz =  fld%num_phys_viz + 1
        fld%ntot_phys_viz = fld%ntot_phys_viz + numdir
!
        call alloc_phys_name_type(fld)
        call add_field_name_list_4_viz                                  &
     &     (field_name, numdir, flag_monitor, iorder_eletype,           &
     &      tmp_fld, fld)
      else
        call alloc_phys_name_type(fld)
        call add_field_name_list_noviz                                  &
     &     (field_name, numdir, flag_monitor, iorder_eletype,           &
     &      tmp_fld, fld)
      end if
!
      call dealloc_phys_name_type(tmp_fld)
!
      end subroutine append_field_name_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_field_name_type(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
!
      new_fld%num_phys =  org_fld%num_phys
      new_fld%ntot_phys = org_fld%ntot_phys
!
      new_fld%num_phys_viz =  org_fld%num_phys_viz
      new_fld%ntot_phys_viz = org_fld%ntot_phys_viz
!
      call alloc_phys_name_type(new_fld)
!
      new_fld%num_component(1:new_fld%num_phys)                         &
     &             = org_fld%num_component(1:new_fld%num_phys)
      new_fld%phys_name(1:new_fld%num_phys)                             &
     &             = org_fld%phys_name(1:new_fld%num_phys)
      new_fld%flag_monitor(1:new_fld%num_phys)                          &
     &             = org_fld%flag_monitor(1:new_fld%num_phys)
      new_fld%iorder_eletype(1:new_fld%num_phys)                        &
     &             = org_fld%iorder_eletype(1:new_fld%num_phys)
      new_fld%istack_component(0:new_fld%num_phys)                      &
     &             = org_fld%istack_component(0:new_fld%num_phys)
!
      end subroutine copy_field_name_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_field_data_type(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
      call alloc_phys_data_type(org_fld%n_point, new_fld)
      new_fld%iflag_update(1:new_fld%ntot_phys)                         &
     &           = org_fld%iflag_update(1:new_fld%ntot_phys)
!
!$omp parallel workshare
      new_fld%d_fld(1:new_fld%n_point,1:new_fld%ntot_phys)              &
     &          = org_fld%d_fld(1:new_fld%n_point,1:new_fld%ntot_phys)
!$omp end parallel workshare
!
      end subroutine copy_field_data_type
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_list_4_viz                              &
     &         (field_name, numdir, flag_monitor, iorder_eletype,       &
     &          org_fld, new_fld)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: iorder_eletype
      logical, intent(in) :: flag_monitor
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
      integer(kind = kint) :: i
!
!
      do i = 1, org_fld%num_phys_viz
        new_fld%num_component(i) = org_fld%num_component(i)
        new_fld%phys_name(i) = org_fld%phys_name(i)
        new_fld%flag_monitor(i) = org_fld%flag_monitor(i)
        new_fld%iorder_eletype(i) = org_fld%iorder_eletype(i)
      end do
!
      new_fld%num_component(new_fld%num_phys_viz) = numdir
      new_fld%phys_name(new_fld%num_phys_viz) =     field_name
      new_fld%flag_monitor(new_fld%num_phys_viz) = flag_monitor
      new_fld%iorder_eletype(new_fld%num_phys_viz) = iorder_eletype
!
      do i = new_fld%num_phys_viz+1, new_fld%num_phys
        new_fld%num_component(i) = org_fld%num_component(i-1)
        new_fld%phys_name(i) = org_fld%phys_name(i-1)
        new_fld%flag_monitor(i) = org_fld%flag_monitor(i-1)
        new_fld%iorder_eletype(i) = org_fld%iorder_eletype(i-1)
      end do
!
      new_fld%istack_component(0) = 0
      do i = 1, new_fld%num_phys
        new_fld%istack_component(i) = new_fld%istack_component(i-1)     &
     &                               + new_fld%num_component(i)
      end do
!
      end subroutine add_field_name_list_4_viz
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_list_noviz                              &
     &         (field_name, numdir, flag_monitor, iorder_eletype,       &
     &          org_fld, new_fld)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: iorder_eletype
      logical, intent(in) :: flag_monitor
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
      integer(kind = kint) :: i
!
!
      do i = 1, org_fld%num_phys
        new_fld%num_component(i) = org_fld%num_component(i)
        new_fld%phys_name(i) = org_fld%phys_name(i)
        new_fld%flag_monitor(i) = org_fld%flag_monitor(i)
        new_fld%iorder_eletype(i) = org_fld%iorder_eletype(i)
      end do
!
      new_fld%num_component(new_fld%num_phys) = numdir
      new_fld%phys_name(new_fld%num_phys) =     field_name
      new_fld%flag_monitor(new_fld%num_phys) = flag_monitor
      new_fld%iorder_eletype(new_fld%num_phys) = iorder_eletype
!
      new_fld%istack_component(0) = 0
      do i = 1, new_fld%num_phys
        new_fld%istack_component(i) = new_fld%istack_component(i-1)     &
     &                               + new_fld%num_component(i)
      end do
!
      end subroutine add_field_name_list_noviz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function field_id_by_address(fld, i_ref)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_ref
!
      integer(kind = kint) :: i, i_start
!
      field_id_by_address = 0
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. i_ref) then
          field_id_by_address = i
          exit
        end if
      end do
!
      end function field_id_by_address
!
! -----------------------------------------------------------------------
!
      character(len=kchara) function field_name_by_address(fld, i_ref)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_ref
!
      integer(kind = kint) :: i, i_start
!
      write(field_name_by_address,'(a)') 'MISSING'
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. i_ref) then
          field_name_by_address = fld%phys_name(i)
          exit
        end if
      end do
!
      end function field_name_by_address
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_all_field_data(id_rank, fld)
!
      integer, intent(in) :: id_rank
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: id
!
      id = 50 + id_rank
!

      call check_nodal_field_name_type(id, fld)
      call check_nodal_data(id, fld, fld%ntot_phys, ione)
!
      end subroutine check_all_field_data
!
!  --------------------------------------------------------------------
!
      subroutine check_nodal_field_name_type(id_output, fld)
!
      integer(kind = kint), intent(in) :: id_output
      type(phys_data), intent(in) :: fld
!
!
      integer(kind = kint) :: i
!
      write(id_output,*) 'fld%num_phys ',fld%num_phys
      write(id_output,*) 'fld%num_phys_viz ',fld%num_phys_viz
      write(id_output,*)                                                &
     &      'id#, num_component, stack_component, field_name '
      do i = 1, fld%num_phys
        write(id_output,'(3i6,2x,a2,a)') i, fld%num_component(i),       &
     &         fld%istack_component(i), '  ', trim(fld%phys_name(i))
      end do
!
      end subroutine check_nodal_field_name_type
!
!   ---------------------------------------------------------------------
!
      subroutine check_nodal_data(id_output, fld, numdir, i_field)
!
      integer(kind = kint), intent(in) :: id_output, numdir, i_field
      type(phys_data), intent(in) :: fld
      integer(kind = kint) :: inod, nd
!
      write(id_output,*) 'inod, nodal field: ', i_field, numdir
      do inod = 1, fld%n_point
        write(id_output,'(i16,1p10e25.14)')                             &
     &         inod, (fld%d_fld(inod,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_nodal_data
!
!  --------------------------------------------------------------------
!
      end module t_phys_data
