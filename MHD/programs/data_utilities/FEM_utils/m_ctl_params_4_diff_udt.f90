!
!      module m_ctl_params_4_diff_udt
!
      module m_ctl_params_4_diff_udt
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
      use m_geometry_constants
      use t_file_IO_parameter
!
      implicit none
!
      character(len = kchara) :: grouping_mesh_head = "grouping_mesh"
!
      character(len = kchara), parameter                                &
     &               :: ref_udt_file_head = "field/out"
      character(len = kchara), parameter                                &
     &               :: tgt_udt_file_head = "field_new/out"
!
      type(field_IO_params), save :: first_ucd_param
      type(field_IO_params), save :: second_ucd_param
      type(field_IO_params), save :: diff_ucd_param
      type(field_IO_params), save :: ave_ucd_param
!
      character(len = kchara), parameter                                &
     &               :: diff_udt_file_head = "field_diff/out"
      character(len = kchara) :: ave_udt_file_head =  "out_average"
!
      character(len = kchara) :: product_field_name = "velocity"
      integer(kind = kint) :: i_field_4_product = 1
      integer(kind = kint) :: ncomp_4_product =   3
      integer(kind = kint) :: icomp_4_product =   3
!
      character(len = kchara) :: correlate_field_name = "velocity"
      character(len = kchara) :: correlate_comp_name =  "norm"
      integer(kind = kint) :: i_field_4_correlate = 1
      integer(kind = kint) :: ncomp_4_correlate =   3
      integer(kind = kint) :: icomp_4_correlate =   3
!
      integer(kind = kint) :: iflag_correlate_coord = iflag_certecian
!
!  ---------------------------------------------------------------------
!
!      contains
!
!   --------------------------------------------------------------------
!
      end module m_ctl_params_4_diff_udt
