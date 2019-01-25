!>@file   t_ctl_param_partitioner.f90
!!@brief  module t_ctl_param_partitioner
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief parpameters for domain partitioning
!!
!!@verbatim
!!      subroutine alloc_rcb_directions(part_p)
!!      subroutine alloc_ele_grp_ordering(part_p)
!!      subroutine alloc_ele_grp_layer_name(part_p)
!!
!!      subroutine dealloc_rcb_directions(part_p)
!!      subroutine dealloc_ele_grp_ordering(part_p)
!!      subroutine dealloc_ele_grp_layer_name(part_p)
!!        type(ctl_param_partitioner), intent(inout) :: part_p
!!@endverbatim
!
      module t_ctl_param_partitioner
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
!
      integer(kind = kint), parameter :: iPART_RCB_XYZ = 1
      integer(kind = kint), parameter :: iPART_RCB_SPH = 2
!
      integer(kind = kint), parameter :: iPART_EQ_XYZ =  8
      integer(kind = kint), parameter :: iPART_EQV_XYZ =  11
      integer(kind = kint), parameter :: iPART_EQ_SPH =  9
      integer(kind = kint), parameter :: iPART_LAYER_SPH = 10
!
      integer(kind = kint), parameter :: iPART_MeTiS_RSB = 3
      integer(kind = kint), parameter :: iPART_GEN_MeTiS = 4
!
      integer(kind = kint), parameter :: iPART_CUBED_SPHERE =   5
      integer(kind = kint), parameter :: iPART_FINE_MESH_TBL =  6
      integer(kind = kint), parameter :: iPART_DECMP_MESH_TBL = 7
      integer(kind = kint), parameter :: iPART_SPH_TBL_MEM =   17
!
      character(len=kchara), parameter                                  &
     &                      :: def_finer_mesh =    "finer_mesh/in"
!
!
      type ctl_param_partitioner
        integer(kind = kint) :: ndivide_eb(3)
!
        integer(kind = kint) :: NPOWER_rcb
        integer(kind = kint), allocatable :: idir_rcb(:)
!
        integer(kind = kint) :: num_egrp_layer
        character(len=kchara), allocatable :: grp_layer_name(:)
!
        integer(kind = kint) :: nele_grp_ordering = 0
        character(len=kchara), allocatable :: ele_grp_ordering(:)
!
        character(len=kchara) :: fname_subdomain = 'subdomain_table.dat'
!
        character(len=kchara) :: metis_file_name = 'graph.in'
        character(len=kchara) :: metis_sdom_name ='metis_part.dat'
!
        character(len=kchara) :: finer_inter_file_head
        character(len=kchara) :: sphere_data_file_name
      end type ctl_param_partitioner
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_rcb_directions(part_p)
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      allocate(part_p%idir_rcb(part_p%NPOWER_rcb))
      if(part_p%NPOWER_rcb .gt. 0) part_p%idir_rcb = 300
!
      end subroutine alloc_rcb_directions
!
!   --------------------------------------------------------------------
!
      subroutine alloc_ele_grp_ordering(part_p)
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      allocate(part_p%ele_grp_ordering(part_p%nele_grp_ordering))
!
      end subroutine alloc_ele_grp_ordering
!
!   --------------------------------------------------------------------
!
      subroutine alloc_ele_grp_layer_name(part_p)
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      allocate(part_p%grp_layer_name(part_p%num_egrp_layer))
!
      end subroutine alloc_ele_grp_layer_name
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_rcb_directions(part_p)
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
      deallocate(part_p%idir_rcb)
!
      end subroutine dealloc_rcb_directions
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_grp_ordering(part_p)
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      deallocate(part_p%ele_grp_ordering)
!
      end subroutine dealloc_ele_grp_ordering
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ele_grp_layer_name(part_p)
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      deallocate(part_p%grp_layer_name)
!
      end subroutine dealloc_ele_grp_layer_name
!
!   --------------------------------------------------------------------
!
      end module t_ctl_param_partitioner
