#
# Copyright (c) 2020 Michael Roe
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.

.global _start
_start:
	lui	ra, 2
	csrs	mstatus, ra
	fmv.w.x	ft0, zero
	fmv.w.x	ft1, zero
	fmv.w.x	ft2, zero
	fmv.w.x	ft3, zero
	fmv.w.x	ft4, zero
	fmv.w.x	ft5, zero
	fmv.w.x	ft6, zero
	fmv.w.x	ft7, zero
	fmv.w.x	fs0, zero
	fmv.w.x	fs1, zero
	fmv.w.x	fa0, zero
	fmv.w.x	fa1, zero
	fmv.w.x	fa2, zero
	fmv.w.x	fa3, zero
	fmv.w.x	fa4, zero
	fmv.w.x	fa5, zero
	fmv.w.x	fa6, zero
	fmv.d.x	fa7, zero
	fmv.d.x	fs2, zero
	fmv.d.x	fs3, zero
	fmv.d.x	fs3, zero
	fmv.d.x	fs4, zero
	fmv.d.x	fs5, zero
	fmv.d.x	fs6, zero
	fmv.d.x	fs7, zero
	fmv.d.x	fs8, zero
	fmv.d.x	fs9, zero
	fmv.d.x	fs10, zero
	fmv.d.x	fs11, zero
	fmv.d.x	ft8, zero
	fmv.d.x	ft9, zero
	fmv.d.x	ft10, zero
	fmv.d.x	ft11, zero
.data
	.align 16
	.global tohost
tohost:
	.dword 0
	.global fromhost
fromhost:
	.dword 0
